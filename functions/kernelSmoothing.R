# kernel smoothing functions
library(terra)
library(sf)
library(dplyr)
library(torch)

# CSHS utility functions -------------------------------------------------- ####

# Device / dtype helpers (centralise torch choices)
torch_device <- function() "cuda"
torch_dtype  <- function() torch::torch_float()

# numerical stability functions ------------------------------------------- #### 
# Compute row-wise stable exp weights for exponential kernel
# a = -dist_mat * alpha (matrix); returns weights exp(a - max_row)
exp_weights_stable <- function(a_mat) {
  m <- apply(a_mat, 1, max)
  exp(a_mat - m)
}
softplus <- function(x) log1p(exp(-abs(x))) + pmax(x, 0)
softplus_inv <- function(y) ifelse(y > 20, y, log(expm1(y)))

# link functions ---------------------------------------------------------- ####
# sigmoid
sigmoid <- function(x) 1 / (1 + exp(-x))

# logit equation
logit <- function(x) {
  log(x / (1 - x))
}
# inverse logit function
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

# compute_euclidean_distances_torch --------------------------------------- ####
# commpute Euclidean distance matrix (n_focal x n_hab) as a torch tensor.
# x_focal, y_focal: 1D tensors of focal-point coordinates.
# cov_xy: (n_hab x 2) tensor of habitat-cell coordinates.
compute_euclidean_distances_torch <- function(x_focal, y_focal, cov_xy) {
  torch::torch_sqrt(torch::torch_clamp(
    (x_focal$unsqueeze(2) - cov_xy[, 1]$unsqueeze(1))^2 +
      (y_focal$unsqueeze(2) - cov_xy[, 2]$unsqueeze(1))^2,
    min = 0
  ))
}

# GPU potential height at xy ---------------------------------------------- ####
potential_GPU <- function(
    cov_value, # spatRaster, Vector, or matrix of habitat values 
    xy = NULL,  # Matrix of focal points (n x 2)
    cov_xy = NULL, # Dataframe of habitat cell coordinates (x, y)
    alphas, # Distance decay parameter (>0)
    shape = c("exp", "gaus", "thresh", "lin", "inv", "invsq", "epan"), # Distance weighting function ('exp', 'gaus', etc.)
    dist_mat = NULL, # Optional precomputed pairwise distances (n_focal x n_habitat)
    rescale_var = FALSE,  # whether to rescale covariates after smoothing
    max_dist = NULL,  # Maximum distance for calculations (same units as x/y)
    batch_size = NULL,  #  Number of focal points per batch (auto-chosen if NULL)
    habitat_tile = NULL,  # tile habitat when max_dist is NULL
    method = c("fft", "direct"),  # smoothing method
    device = c("cuda", "cpu"),
    dtype = c("float", "half"),  # use "half" on CUDA to save VRAM,
    ...
) {
  shape <- .validate_kernel_shape(shape)
  device <- rlang::arg_match(device)
  dtype  <- rlang::arg_match(dtype)
  method  <- rlang::arg_match(method)
  
  if (!all(as.numeric(alphas) > 0)) 
    cli::cli_abort(c("x" = "All values of {.arg alphas} must be {.code  > 0}"))
  
  # If cov_value is terra SpatRaster
  as_spatRaster <- inherits(cov_value, "SpatRaster")
  if (as_spatRaster) {
    if (is.null(cov_xy)) {
      cov_xy <- as.data.frame(terra::as.data.frame(cov_value, xy = TRUE)[, 1:2])
    }
    cov_value_copy <- cov_value
    cov_value <- terra::values(cov_value)  # nrow(rast) × nlyr matrix
  }
  
  # if (method == "fft" & !as_spatRaster) {
  #   cli::cli_abort(c("x" = "{.code method = 'fft'} requires {.arg cov_value} to be a {.cls SpatRaster}."))
  # }
  
  if (method == "fft") {
    # check if unuccesary arguments defined when fft
    if (!is.null(dist_mat)) {
      cli::cli_warn(c("{.arg dist_mat} cannot be used when {.code method = 'fft'}.",
                      "i" = "Set {.arg method} to {.val direct} to use a precomputed distance matrix {.arg dist_mat}.",
                      "i" = "Ignoring {.arg dist_mat}."))
    }
    if (dtype == "half") {
      cli::cli_warn(c("{.arg dtype} must be {.val float} used when {.code method = 'fft'}.",
                      "i" = "Set {.arg method} to {.val direct} to use half-precision floating-point format.",
                      "i" = "Assuming {.code dtype = 'float'}."))
      dtype <- "float"
    }
    # determine whether to extract estimates at focal coordinates when xy differs from full raster grid
    extract_smooth_xy <- !is.null(xy)
    xy_copy <- xy
  } else {
    extract_smooth_xy <- FALSE
  }
  
  # if xy not provided, assume smoothing entire raster
  if (is.null(xy)) xy <- cov_xy
  
  # Choose torch dtype
  torch_dtype <- if (dtype == "half" && device == "cuda") torch_half() else torch_float()
  
  # convert arguments to tensors (on target device)
  xy <-         if (inherits(xy, "torch_tensor"))        xy        else torch_tensor(as.matrix(xy),      device = device, dtype = torch_dtype)
  cov_xy <-     if (inherits(cov_xy, "torch_tensor"))    cov_xy    else torch_tensor(as.matrix(cov_xy),  device = device, dtype = torch_dtype)
  cov_value <-  if (inherits(cov_value, "torch_tensor")) cov_value else torch_tensor(cov_value,          device = device, dtype = torch_dtype)
  alphas <-     if (inherits(alphas, "torch_tensor"))    alphas    else torch_tensor(as.numeric(alphas), device = device, dtype = torch_dtype)
  
  # smoothing with FFT 
  if (method == "fft") {
    
    # get raster attributes
    n_cov <- terra::nlyr(cov_value_copy)
    nrows <- terra::nrow(cov_value_copy)
    ncols <- terra::ncol(cov_value_copy)
    res_row <- terra::yres(cov_value_copy)
    res_col <- terra::xres(cov_value_copy)
    
    # recycled alpha (one per layer; replicate if only one supplied)
    alpha_vec <- as.numeric(alphas)
    if (length(alpha_vec) == 1L) alpha_vec <- rep(alpha_vec, n_cov)
    if (length(alpha_vec) != n_cov)
      cli::cli_abort(c("x" = "Length of {.arg alphas} ({length(alpha_vec)}) must be 1 or equal to the number of raster layers ({n_cov})."))
    
    # pre-allocate output: nrows × ncols × n_cov tensor
    smoothed_t <- torch::torch_zeros(c(nrows, ncols, n_cov),
                                     device = device, dtype = torch::torch_float())
    
    # smooth each covariate FFT
    for (k in seq_len(n_cov)) {
      # reshape layer into 2D spatial grid 
      layer_vals <- cov_value[, k]$reshape(c(nrows, ncols))  # (nrows × ncols)
      # build kernel for layer
      krad_row <- .fft_kernel_radius(alpha = alpha_vec[k], shape = shape,
                                     res = res_row, max_dist = max_dist, ...)
      krad_col <- .fft_kernel_radius(alpha = alpha_vec[k], shape = shape,
                                     res = res_col, max_dist = max_dist, ...)
      kernel <- .build_fft_kernel_torch(alpha = alpha_vec[k], shape = shape, 
                                        krad_row = krad_row, krad_col = krad_col,
                                        res_row = res_row, res_col = res_col,
                                        device = device)
      # normalise kernel weight to sum to 1
      kernel <- kernel / kernel$sum()
      # convolve
      smoothed_t[, , k] <- .fft_smooth(x = layer_vals, kernel = kernel,
                                       device = device) 
    }
    
    # reshape & rename for output
    out_h <- smoothed_t$reshape(c(nrows * ncols, n_cov))
  } else {
    # Ensure cov_value is 2D
    if (cov_value$dim() == 1) cov_value <- cov_value$unsqueeze(2)
    # number of covariates
    n_cov <- cov_value$size(2)
    
    # check/expand alphas
    if (!length(alphas) %in% c(1, n_cov))
      cli::cli_abort(c("x" = "Length of {.arg alphas} must be 1 or match the number of covariates."))
    
    if (length(alphas) < n_cov) alphas <- alphas$`repeat`(n_cov)
    
    # NA-aware Center habitat covariates once
    cv_unsq <- (cov_value - cov_value$nanmean(dim = 1, keepdim = TRUE))$unsqueeze(1)
    
    # outputs
    n_focal <- xy$size(1)
    out_h <- torch_empty(c(n_focal, n_cov), device = device, dtype = torch_dtype)
    
    # Heuristic batch size if not provided (caps peak memory: batch_size * n_habitat tensors)
    if (is.null(batch_size)) batch_size <- min(4096L, as.integer(n_focal))
    if (is.null(habitat_tile)) habitat_tile <- if (is.null(max_dist)) 65536L else NULL  # tile only if global distances
    
    # main loops ----------------
    if (!is.null(dist_mat)) {
      # Pre-computed distance matrix path
      if (!inherits(dist_mat, "torch_tensor")) {
        dist_mat <- torch_tensor(as.matrix(dist_mat), device = device, dtype = torch_dtype)
      }
      if (dist_mat$size(1) != n_focal || dist_mat$size(2) != cov_xy$size(1)) 
        cli::cli_abort(c("x" = "{.arg dist_mat} must have dimensions (n_focal x n_habitat)."))
      
      start_idx <- 1L
      while (start_idx <= n_focal) {
        end_idx <- min(n_focal, start_idx + batch_size - 1L)
        idx <- start_idx:end_idx
        out_h[idx, ] <- .kernel_weighted_heights_torch(dist_mat[idx, ], cv_unsq, alphas, shape)
        start_idx <- end_idx + 1L
      }
    } else {
      n_hab <- cov_xy$size(1)
      if (is.null(max_dist) && !is.null(habitat_tile) && habitat_tile < n_hab) {
        # Global kernel with habitat tiling to cap peak memory.
        # Raw (un-stabilised) weights are accumulated across tiles; all three kernels
        # return values in (0, 1] so there is no overflow/underflow risk.
        # .apply_kernel_torch() is called with stabilise=FALSE so that weights from
        # different tiles remain on the same scale and can be summed correctly.
        start_idx <- 1L
        while (start_idx <= n_focal) {
          end_idx <- min(n_focal, start_idx + batch_size - 1L)
          idx <- start_idx:end_idx
          b <- length(idx)
          # accumulate numerator/denominator across tiles
          acc_num <- torch_zeros(c(b, n_cov), device = device, dtype = torch_dtype)
          acc_den <- torch_zeros_like(acc_num)
          
          hstart <- 1L
          while (hstart <= n_hab) {
            hend <- min(n_hab, hstart + habitat_tile - 1L)
            sub_cov_xy <- cov_xy[hstart:hend, , drop=FALSE]
            sub_cv_unsq <- cv_unsq[, hstart:hend, , drop=FALSE]    # (1 x n_hsub x n_cov)
            
            # squared distances (b x n_hsub)
            d <- compute_euclidean_distances_torch(xy[idx, 1], xy[idx, 2], sub_cov_xy)
            
            # compute kernel weights and keep track of accumulators
            hend       <- min(n_hab, hstart + habitat_tile - 1L)
            sub_cov_xy <- cov_xy[hstart:hend, , drop = FALSE]
            sub_cv     <- cv_unsq[, hstart:hend, , drop = FALSE]  # (1 x n_hsub x n_cov)
            
            for (k in seq_len(n_cov)) {
              w <- .apply_kernel_torch(d, alphas[k], shape, vm = NULL, stabilise = FALSE)
              acc_num[, k] <- acc_num[, k] + (w * sub_cv[1, , k])$sum(dim = 2)
              acc_den[, k] <- acc_den[, k] + w$sum(dim = 2)
            }
            hstart <- hend + 1L
          }
          out_h[idx, ] <- acc_num / acc_den$clamp_min(1e-20)
          start_idx <- end_idx + 1L
        }
      } else {
        # max_dist path: prune habitat by bounding box per focal batch.
        # plain path (max_dist = NULL): use all habitat cells.
        start_idx <- 1L
        while (start_idx <= n_focal) {
          end_idx <- min(n_focal, start_idx + batch_size - 1L)
          idx <- start_idx:end_idx
          x_batch <- xy[idx, 1]
          y_batch <- xy[idx, 2]
          
          if (!is.null(max_dist)) {
            x_min <- torch_min(x_batch)$item() - max_dist
            x_max <- torch_max(x_batch)$item() + max_dist
            y_min <- torch_min(y_batch)$item() - max_dist
            y_max <- torch_max(y_batch)$item() + max_dist
            
            # boolean mask on cov_xy within expanded bbox — very cheap
            msk <- (cov_xy[,1]$ge(x_min) & cov_xy[,1]$le(x_max) &
                      cov_xy[,2]$ge(y_min) & cov_xy[,2]$le(y_max))
            if (!msk$any()$item()) {
              # No neighbors at all -> return zeros (already centered), keep going
              out_h[idx, ] <- torch_zeros(c(length(idx), n_cov), device = device, dtype = torch_dtype)
              start_idx <- end_idx + 1L
              next
            } 
            
            # else, if bbox prunes everything (rare), skip safely
            sub_cov_xy <- cov_xy[msk, , drop = FALSE]
            sub_cv_unsq <- cv_unsq[, msk, , drop = FALSE]
            d  <- compute_euclidean_distances_torch(x_batch, y_batch, sub_cov_xy)
            vm <- d$le(max_dist)
            out_h[idx, ] <- .kernel_weighted_heights_torch(d, sub_cv_unsq, alphas, shape, vm)
          } else {
            d <- compute_euclidean_distances_torch(x_batch, y_batch, cov_xy)
            out_h[idx, ] <- .kernel_weighted_heights_torch(d, cv_unsq, alphas, shape)
            sub_cov_xy <- cov_xy
            sub_cv_unsq <- cv_unsq
          }
          start_idx <- end_idx + 1L
        }
      }
    }
  }
  
  # Variance rescaling
  if (rescale_var) {
    valid_mask  <- out_h$isnan()$logical_not()$to(dtype = torch_float())
    valid_sum   <- valid_mask$sum(dim = 1L, keepdim = TRUE)$clamp_min(1)  # (1 x n_cov)
    out_filled  <- torch_where(out_h$isnan(), torch_zeros_like(out_h), out_h)
    mu  <- (out_filled * valid_mask)$sum(dim = 1L, keepdim = TRUE) / valid_sum
    var <- ((out_filled - mu)^2 * valid_mask)$sum(dim = 1L, keepdim = TRUE) /
      (valid_sum - 1)$clamp_min(1)
    sd  <- var$sqrt()$clamp_min(1e-8)
    out_h <- (out_h - mu) / sd
  }
  
  # to CPU for return
  out <- as.array(out_h$to(device = "cpu", dtype = torch_float()))  # return float on CPU
  
  if (as_spatRaster) {
    terra::values(cov_value_copy) <- out
    # extract smoothed values at specified coordinates 
    if (extract_smooth_xy) {
      terra::extract(cov_value_copy, xy_copy)
    } else {
      cov_value_copy
    }
  } else {
    if (n_cov == 1) as.vector(out) else out
  }
}

# create prediction raster from fitted model ------------------------------ ####
predict_cshs_raster <- function(object,
                                covs,
                                type  = c("link", "response", "rss"),
                                scale = c("natural", "working"),
                                cumulative = TRUE,
                                rescale_var = NULL,
                                intercept = TRUE,
                                covariate_values = NULL,
                                ...) {
  # match arguments
  scale <- rlang::arg_match(scale)
  type  <- rlang::arg_match(type)
  
  # extract coefficients on desired scale
  coef <- coef(object, scale = scale)
  
  # model_spec should contain:
  # - terms:         model formula (with smooth() already encoded)
  # - beta_cols:     names of columns in the model matrix that have β's
  # - smoothed_vars: names/labels of smoothed spatial covariates or expressions
  # - smoothed_exprs:list of unevaluated smoothed expressions (for prediction)
  spec <- object$model_spec
  if (is.null(spec)) {
    stop("Model specification missing from fitted object; cannot build prediction design matrix.")
  }
  
  # dummy smooth() so terms() / model.matrix() recognise it if needed
  smooth <- function(x) x
  
  # choose intercept value for prediction
  if (intercept) {
    intercept <- unname(coef["(Intercept)"])
  } else {
    intercept <- 0
  }
  
  # β coefficients in the same order as spec$beta_cols
  betas <- if (length(spec$beta_cols)) coef[spec$beta_cols] else numeric()
  
  # work out which variables are spatial and which (if any) are non-spatial
  smoothed_vars <- spec$smoothed_vars
  smoothed_exprs <- spec$smoothed_exprs
  unsmoothed_vars <- spec$unsmoothed_vars
  response_col <- spec$response_col
  all_vars      <- setdiff(all.vars(spec$terms), response_col)
  spatial_vars  <- intersect(all_vars, names(covs))
  nonspatial    <- setdiff(all_vars, spatial_vars)
  
  # Resolve values for covariates not present as raster layers.
  # Priority: covariate_values argument > mean of training data > error.
  nonspatial_vals <- list()
  if (length(nonspatial)) {
    unresolved <- character()
    for (v in nonspatial) {
      if (!is.null(covariate_values) && v %in% names(covariate_values)) {
        nonspatial_vals[[v]] <- covariate_values[[v]]
      } else if (!is.null(object$data) && v %in% names(object$data)) {
        val <- mean(object$data[[v]], na.rm = TRUE)
        message("Using mean value for non-spatial covariate '", v, "': ", round(val, 4))
        nonspatial_vals[[v]] <- val
      } else {
        unresolved <- c(unresolved, v)
      }
    }
    if (length(unresolved)) {
      stop(
        "Prediction requires all model covariates to be present as raster layers or ",
        "supplied via 'covariate_values': ",
        paste(unresolved, collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  # decide whether to use variance–rescaling (same choice as in fitting by default)
  if (is.null(rescale_var)) {
    rescale_var <- isTRUE(object$scaling$rescale_var)
  }
  
  # Build a stack of spatial predictors used in the model:
  #   - smoothed covariates: run through potential_GPU with fitted α's
  #   - unsmoothed covariates: raw covariate rasters
  
  # smooth only the covariates that were smoothed in the model
  smoothed_stack <- NULL
  if (length(smoothed_vars)) {
    # Natural-scale coef stores alphas under display labels (e.g. "alpha_c1",
    # "alpha_c1.1"); working-scale coef stores them under internal labels with
    # shape suffix (e.g. "theta_alpha_c1__exp") and needs softplus to recover α.
    # smoothed_vars holds internal labels; smoothed_display_vars holds display
    display_vars <- if (!is.null(spec$smoothed_display_vars)) spec$smoothed_display_vars else smoothed_vars
    if (scale == "natural") {
      alpha_names <- paste0("alpha_", display_vars)
      if (!all(alpha_names %in% names(coef))) {
        stop("Alpha parameters for smoothed covariates are missing from the fitted model coefficients.")
      }
      alphas <- coef[alpha_names]
    } else {
      theta_names <- paste0("theta_alpha_", smoothed_vars)
      if (!all(theta_names %in% names(coef))) {
        stop("Alpha parameters for smoothed covariates are missing from the fitted model coefficients.")
      }
      alphas <- softplus(coef[theta_names])
    }
    
    # Evaluate each smoothed expression (e.g., I(1 - forest), forest^2) on the
    # supplied covariate stack to build the raster(s) fed into potential_GPU.
    if (is.null(smoothed_exprs)) {
      smoothed_exprs <- lapply(smoothed_vars, rlang::parse_expr)
    }
    raw_smoothed_stack <- terra::rast(
      lapply(seq_along(smoothed_exprs), function(i) {
        expr <- smoothed_exprs[[i]]
        lbl  <- smoothed_vars[[i]]
        base_layers <- all.vars(expr)
        if (!all(base_layers %in% names(covs))) {
          stop("All variables inside smooth() must be provided as raster layers for prediction.")
        }
        layer <- terra::lapp(covs[[base_layers]], fun = function(...) {
          vals <- list(...)
          names(vals) <- base_layers
          out <- eval(expr, envir = vals)
          if (inherits(out, "AsIs")) {
            out <- unclass(out)
          }
          out
        })
        names(layer) <- lbl
        layer
      })
    )
    
    # Per-covariate kernel shapes from model spec
    shapes <- spec$smoothed_shapes[smoothed_vars]
    
    # Apply each covariate's decay kernel independently so that different
    # shapes (e.g. "exp" vs "gaus") are honoured per term.
    smoothed_stack <- terra::rast(
      lapply(seq_along(smoothed_vars), function(i) {
        lyr <- potential_GPU(
          cov_value   = raw_smoothed_stack[[i]],
          alphas      = alphas[[i]],
          shape         = shapes[[i]],
          rescale_var = rescale_var,
          ...
        )
        names(lyr) <- smoothed_vars[[i]]
        lyr
      })
    )
    names(smoothed_stack) <- smoothed_vars
  }
  
  # unsmoothed spatial covariates (used as-is in the model)
  smoothed_base_vars <- if (length(smoothed_exprs)) unique(unlist(lapply(smoothed_exprs, all.vars))) else character()
  only_smoothed <- setdiff(smoothed_base_vars, unsmoothed_vars)  # identify vars that are only smoothed
  unsmoothed_spatial <- setdiff(spatial_vars, only_smoothed)  # identify spatials that are not smoothd
  
  # combine smoothed + unsmoothed into a single stack used for prediction
  model_stack <- NULL
  if (!is.null(smoothed_stack)) {
    model_stack <- smoothed_stack
  }
  if (length(unsmoothed_spatial)) {
    unsmoothed_stack <- covs[[unsmoothed_spatial]]
    model_stack <- if (is.null(model_stack)) {
      unsmoothed_stack
    } else {
      c(model_stack, unsmoothed_stack)
    }
  }
  
  if (is.null(model_stack)) {
    stop("No spatial covariate layers available for prediction.")
  }
  
  # give each smoothed term a unique, syntactic alias so prediction covariates use
  # distinct column names (avoids re-evaluating expressions and colliding with
  # unsmoothed variables). Aliases keep prediction design matrices aligned with
  # fitted β names without any additional transformations.
  sm_alias <- if (length(smoothed_vars)) paste0(".smooth_", seq_along(smoothed_vars)) else character()
  
  # Build model matrix for prediction, using the same formula as in fitting.
  # model_stack values provide the covariates; model.matrix() encodes all
  # main effects and interactions (including smooth(...) if used in terms).
  #
  # To keep mapping between fitted β names (which include smooth(...)) and
  # prediction design columns, track any renaming applied to smoothed layers
  # (e.g., adding a suffix when a base covariate also appears unsmoothed).
  if (!is.null(smoothed_stack)) {
    names(smoothed_stack) <- sm_alias
    names(model_stack)[seq_along(sm_alias)] <- sm_alias
  }
  
  cov_df <- as.data.frame(model_stack, xy = FALSE)
  
  # Add constant columns for non-spatial variables resolved above
  for (v in names(nonspatial_vals)) {
    cov_df[[v]] <- nonspatial_vals[[v]]
  }
  
  if (is.null(spec$mm_info)) {
    stop(
      "Model specification missing design information for prediction; please refit the model.",
      call. = FALSE
    )
  }
  
  sm_lookup <- if (length(smoothed_vars)) setNames(sm_alias, smoothed_vars) else character()
  has_intercept <- isTRUE(spec$mm_has_intercept)
  build_mm_col <- function(parts) {
    col_vec <- rep(1, nrow(cov_df))
    for (pt in parts) {
      if (pt$smoothed) {
        col_nm <- sm_lookup[[pt$var]]
        if (is.null(col_nm) || !col_nm %in% names(cov_df)) {
          stop(
            "Prediction requires smoothed covariate '", pt$var, "' in the prediction stack.",
            call. = FALSE
          )
        }
        col_vec <- col_vec * cov_df[[col_nm]]^pt$power
      } else {
        if (!is.null(pt$poly)) {
          x <- tryCatch(
            as.numeric(eval(str2lang(pt$poly$expr_txt), envir = as.list(cov_df))),
            error = function(e) {
              stop(
                "Prediction requires polynomial covariate expression '",
                pt$poly$expr_txt, "' in the prediction stack.",
                call. = FALSE
              )
            }
          )
          poly_vals <- if (isTRUE(pt$poly$raw)) {
            x^pt$poly$basis
          } else {
            if (is.null(pt$poly$coefs)) {
              stop(
                "Polynomial coefficients are missing from the fitted model; please refit before predicting.",
                call. = FALSE
              )
            }
            as.numeric(stats::poly(
              x,
              degree = pt$poly$degree,
              coefs = pt$poly$coefs
            )[, pt$poly$basis])
          }
          col_vec <- col_vec * poly_vals
          next
        }
        if (pt$var %in% names(cov_df)) {
          col_vec <- col_vec * cov_df[[pt$var]]
        } else {
          # Derived expression (e.g. I(elevation^2)) - evaluate against cov_df
          col_vec <- tryCatch(
            col_vec * as.numeric(eval(str2lang(pt$var), envir = as.list(cov_df))),
            error = function(e) {
              stop(
                "Prediction requires covariate '", pt$var, "' in the prediction stack.",
                call. = FALSE
              )
            }
          )
        }
      }
    }
    col_vec
  }
  
  mm_cols <- lapply(spec$mm_info$components, build_mm_col)
  mm_no_int <- if (length(mm_cols)) {
    out <- do.call(cbind, mm_cols)
    colnames(out) <- spec$mm_info$cols
    out
  } else {
    matrix(nrow = nrow(cov_df), ncol = 0)
  }
  
  # map fitted beta column names to prediction matrix column names
  beta_cols_pred <- spec$beta_cols
  
  # Compute contribution of each term:
  #   cumulative = TRUE:
  #     - eta_spatial = X * β → single raster (sum over all terms)
  #   cumulative = FALSE:
  #     - each column j: X[, j] * β_j → one raster layer per term
  if (length(betas)) {
    # reorder / subset model matrix to match β ordering (spec$beta_cols)
    mm_beta <- mm_no_int[, beta_cols_pred, drop = FALSE]
    
    if (isFALSE(cumulative)) {
      # elementwise multiplication of each column by its β:
      # result is [ncell x n_terms], one column per term
      contrib <- sweep(mm_beta, 2, betas, `*`)
    } else {
      # full linear predictor from spatial part only (vector length ncell)
      contrib <- drop(mm_beta %*% betas)
    }
  } else {
    # no β's: all contributions are zero
    if (isFALSE(cumulative)) {
      contrib <- matrix(
        0,
        nrow = nrow(mm_no_int),
        ncol = ncol(mm_no_int),
        dimnames = list(NULL, colnames(mm_no_int))
      )
    } else {
      contrib <- rep(0, nrow(mm_no_int))
    }
  }
  
  # Convert contributions into SpatRaster(s)
  template <- model_stack[[1]]
  if (isFALSE(cumulative)) {
    # multi-layer raster: one layer per term (β_j * X_j)
    contrib_rast <- terra::rast(template, nlyrs = ncol(contrib))
    contrib_rast <- terra::setValues(contrib_rast, contrib)
    
    # name layers by the corresponding β / column name
    names(contrib_rast) <- colnames(contrib)
    
    # when cumulative = FALSE, interpretation is "per-term contribution":
    #   type = "link" → β_j * X_j
    #   type = "rss"  → exp(β_j * X_j)
    #   type = "response" is not meaningful (would need summed predictor)
    if (type == "link") {
      return(contrib_rast)
    } else if (type == "rss") {
      return(terra::app(contrib_rast, exp))
    } else if (type == "response") {
      cli::cli_abort(c(
        "{.code type = 'response'} is only supported with {.code cumulative = TRUE}."
      ))
    }
  }
  
  # cumulative = TRUE: single raster of the summed spatial linear predictor
  contrib_rast <- terra::setValues(template, contrib)
  
  # add intercept if present and requested
  eta <- contrib_rast
  if (has_intercept && !is.na(intercept)) {
    eta <- eta + intercept
  }
  
  # Map from link scale to requested output:
  #   - "link":    η
  #   - "response": plogis(η) (probability)
  #   - "rss":     exp(η)     (relative selection strength)
  if (type == "link") {
    eta
  } else if (type == "response") {
    terra::app(eta, plogis)
  } else if (type == "rss") {
    terra::app(eta, exp)
  }
}


# generate integration points (sample available habitat) ------------------ ####
sample_avail <- function(domain, n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  if (inherits(domain, "SpatRaster")) {
    # restrict to non-NA area
    m <- !is.na(domain)
    domain <- as.polygons(m, dissolve = TRUE)
  } else if (!inherits(domain, "SpatVector")) {
    stop("Domain must be a SpatRaster or SpatVector.")
  }
  
  # sample n random points inside polygon(s)
  pts <- terra::spatSample(domain, size = n, method = "random") %>% 
    as.data.frame(geom = "XY")
  
  return(data.frame(x = pts$x, y = pts$y, case = 0L))
}

# tidy glm fit ------------------------------------------------------------ ####
tidy_glm <- function(fit) {
  tt <- broom::tidy(fit)  # term, estimate, std.error, statistic, p.value
  tibble::tibble(
    term = tt$term,
    estimate = tt$estimate,
    std.error = tt$std.error,
    lwr = tt$estimate - 1.96 * tt$std.error,
    upr = tt$estimate + 1.96 * tt$std.error,
    statistic = tt$statistic,
    p.value = tt$p.value
  )
}

wrap_glm_as_cshs_fit <- function(fit, model_label) {
  est <- stats::coef(fit)
  vc  <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  new_cshs_fit(
    model = model_label,
    coef = est,
    coef_natural = NULL,
    vcov = vc,
    tidy = tidy_glm(fit),
    metrics = tibble::tibble(model = model_label, 
                             logLik = as.numeric(logLik(fit)), 
                             AIC = AIC(fit),
                             runtime = NULL,
                             converged = isTRUE(fit$converged)),
    message = NULL,
    diagnostics = list(),
    scaling = list(mu = NULL, sigma = NULL, domain = NULL, rescale_var = FALSE),
    call = match.call()
  )
}

# plot decay curve for given alpha ---------------------------------------- ####
plot_decay <- function(object, 
                       shape = c("exp", "gaus", "thresh", "lin", "inv", "invsq", "epan"), 
                       scale = c("natural", "working"), 
                       xlim = NULL, thresh = 0.01,
                       bin_width = NULL,
                       return = FALSE, 
                       beta_weight = FALSE  # whether to weigh the decay by beta coefficients (only when object is class == cshs_fit)
) {
  shape <- .validate_kernel_shape(shape, several.ok = TRUE)
  scale <- rlang::arg_match(scale)
  # get alphas from fitted model
  if (inherits(object, "cshs_fit")) {
    alpha <- get_alpha(object)
    shape <- object$model_spec$smoothed_shapes
    if (beta_weight) {
      beta_full     <- get_beta(object)
      beta_full     <- beta_full[names(beta_full) %!in% object$model_spec$unsmoothed_vars]
      components    <- object$model_spec$mm_info$components
      col_names     <- object$model_spec$mm_info$cols
      smoothed_vars <- object$model_spec$smoothed_vars
      
      beta  <- numeric(length(alpha))
      beta2 <- numeric(length(alpha))          # quadratic term; 0 if not present
      for (i in seq_along(smoothed_vars)) {
        for (j in seq_along(components)) {
          comp <- components[[j]][[1]]
          if (isTRUE(comp$var == smoothed_vars[i] && comp$smoothed)) {
            b <- beta_full[[col_names[j]]]
            if (comp$power == 1) beta[i]  <- b
            if (comp$power == 2) beta2[i] <- b
          }
        }
      }
    } else {
      beta  <- rep(1, length(alpha))
      beta2 <- rep(0, length(alpha))        
    }
  } else {
    # object is a plain alpha vector
    alpha <- object
    beta  <- rep(1, length(alpha))
    beta2 <- rep(0, length(alpha))
  }
  
  # check/expand alpha
  if (length(alpha) != length(shape)) {
    if (length(alpha) > 1 && length(shape) > 1)
      stop("alpha and shape must match in length or one must be length 1")
    if (length(alpha) == 1) alpha <- rep(alpha, length(shape)) else shape <- rep(shape, length(alpha))
  }
  
  if (!is.null(xlim)) {
    # fix xlim if just upper
    if (length(xlim) == 1 & max(xlim) > 0) {
      xlim <- c(0, xlim)
    }
  } else {
    # get max upper limit based on alpha, shape, and threshold
    lims <- mapply(function(alpha, shape) 
      alpha_to_dist(alpha, shape, scale, thresh), 
      alpha, shape)
    xlim <- c(0, max(lims))
  }
  
  # rescale alpha
  if (scale == "working") alpha <- softplus(alpha)
  
  # define x values
  if (!is.null(bin_width)) {
    x <- seq(xlim[1], xlim[2], by = bin_width)
    x <- rep(x, each = 2)
  } else {
    x <- seq(xlim[1], xlim[2], length.out = 1000)
  }
  
  params <- tibble::tibble(id = seq_along(alpha), alpha = alpha, shape = shape)
  grid   <- tidyr::expand_grid(id = params$id, x = x) |>
    dplyr::left_join(params, by = "id") |>
    dplyr::rowwise() %>% 
    dplyr::mutate(
      y_raw = .apply_kernel_torch(torch::torch_tensor(x),
                                  torch::torch_tensor(alpha),
                                  shape, stabilise = FALSE) %>% 
        as.numeric(),
      y = y_raw * beta[id] + y_raw^2 * beta2[id],   # beta1*f + beta2*f^2
      cov = if (inherits(object, "cshs_fit")) {
        object$model_spec$smoothed_display_vars[[
          which(names(shape) == names(object$model_spec$smoothed_shapes))
        ]]
      } else { NA },
      lab = paste0(if (inherits(object, "cshs_fit")) cov else names(alpha), 
                   " (", shape,"): ", signif(alpha, 4))
    )
  
  # define ylimits 
  if (!beta_weight) {
    ylim <- c(0, 1)
  } else {
    ylim <- range(grid$y)
  }
  
  # modify x if binned 
  if (!is.null(bin_width)) {
    grid <- mutate(grid, y = lag(y))
  }
  
  # return df if requested
  if (return) {
    return(grid)
  }
  
  # plot
  ggplot2::ggplot(grid, ggplot2::aes(x, y, colour = lab, group = id)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_y_continuous(limits = ylim) +
    ggplot2::labs(x = "distance", y = "weight", colour = "curve")
}


# kernel weighting functions
# kernel smoothing functions
# .apply_kernel_torch ----------------------------------------------------- ####
# Compute unnormalised kernel weights [N x M] for vector of distances and decay par
# use log-sum-exp stabilisation when stabilise = TRUE
# Masked cells receive weight 0 in all cases.
.apply_kernel_torch <- function(dist, alpha, shape, vm = NULL, stabilise = TRUE) {
  log_space <- shape %in% c("exp", "gaus")
  
  # apply decay function
  psi <- switch(shape,
                exp = - dist * alpha,
                gaus = -dist^2  * alpha^2 / 2,
                thresh = (dist <= 1 / alpha)$to(dtype = dist$dtype),
                lin =  torch::torch_clamp(1 - dist * alpha, min = 0),
                inv = 1 / (dist * alpha + 1),
                invsq = 1 / (dist^2 * alpha^2  + 1),
                # Epanechnikov kernel
                epan = torch::torch_clamp(3 / 4 * (1 - (dist * alpha)^2), min = 0)
  )
  # mask cells
  if (!is.null(vm)) {
    psi <- psi$masked_fill(vm$logical_not(),
                           if(log_space) -1e4 else 0)
  }
  # stabilise log-space with log-sum-exp
  if (log_space) {
    max_stab <- if (stabilise) torch::torch_max(psi, dim = 2, 
                                                keepdim = TRUE)[[1]]$detach() else 0
    torch::torch_exp(psi - max_stab)
  } else {
    psi
  }
}

# Compute kernel-weighted average heights for one covariate.
# .kernel_weighted_height_torch <- function(dist, vals_k, alpha, shape, vm, stabilise = TRUE) {
#   psi <- .apply_kernel_torch(dist, alpha, shape, vm, stabilise)
#   num <- (psi * vals_k)$nansum(dim = 2)
#   den <- torch::torch_clamp(psi$sum(dim = 2), min = 1e-20)
#   num / den
# }
.kernel_weighted_height_torch <- function(dist, vals_k, alpha, shape, vm, stabilise = TRUE) {
  psi <- .apply_kernel_torch(dist, alpha, shape, vm, stabilise)
  psi <- psi$masked_fill(vals_k$isnan(), 0)
  
  num <- (psi * vals_k$nan_to_num())$sum(dim = 2)
  den <- psi$sum(dim = 2)
  
  torch::torch_where(
    den$gt(0),
    num / den,
    torch::torch_full_like(den, NaN)
  )
}

# Multi-covariate kernel-weighted average heights.
# dist:  (b x M); vals: (1 x M x K) or (b x M x K); alphas: length-K tensor;
# shapes:  single string or length-K character vector; vm: (b x M) mask or NULL.
# Returns: (b x K) tensor.
.kernel_weighted_heights_torch <- function(dist, vals, alphas, shapes, vm = NULL, stabilise = TRUE) {
  cov_ndim <- length(vals$size())
  K <- vals$size(cov_ndim)
  if (length(shapes) == 1L) shapes <- rep(shapes, K)
  out <- torch::torch_empty(c(dist$size(1), K), device = dist$device, dtype = dist$dtype)
  for (k in seq_len(K)) {
    vals_k <- if (cov_ndim == 3) vals[, , k] else vals[, k]
    out[, k] <- .kernel_weighted_height_torch(dist, vals_k, alphas[k], shapes[k], vm, stabilise)
  }
  out
}

# FFT kernel smoothing ---------------------------------------------------- ####

# build 2D kernel densor for FFT convolution
.build_fft_kernel_torch <- function(alpha, shape, krad_row, krad_col, 
                                    res_row, res_col, device) {
  # kernel height/width (in pixels)  
  # double size and add 1 for centre
  kH <- 2L*krad_row + 1L 
  kW <- 2L*krad_col + 1L
  
  # kernel offset height/width in true units
  row_off <- torch::torch_tensor(seq(-krad_row, krad_row) * res_row,
                                 device = device, dtype = torch::torch_float())
  col_off <- torch::torch_tensor(seq(-krad_col, krad_col) * res_col,
                                 device = device, dtype = torch::torch_float())
  # calculate distance grid
  dist_grid <- torch::torch_sqrt(
    row_off$view(c(kH, 1L))$expand(c(kH, kW))^2 +  # row offset as 1 row
      col_off$view(c(1L, kW))$expand(c(kH, kW))^2)  # col offset as 1 column
  # move alpha to correct device
  alpha_t <- torch::torch_tensor(as.numeric(alpha),
                                 device = device, dtype = torch::torch_float())
  # calculate kernel weight 
  w_flat <- .apply_kernel_torch(dist_grid$reshape(c(1L, kH * kW)),
                                alpha_t, shape, vm = NULL, stabilise = FALSE)
  w_flat$reshape(c(kH, kW))
}

# kernel radius (in cells) 
.fft_kernel_radius <- function (alpha, shape, res, max_dist = NULL, ...) {
  # return number of cells if max_dist is defined
  if (!is.null(max_dist)) return(as.integer(ceiling(max_dist / res))) 
  # else distance at which shape & alpha reach threshold 
  dist <- alpha_to_dist(object = alpha, shape = shape, ...)
  # calculate number of cells given resolution
  as.integer(ceiling(dist / res))
}

# kernel smooth single covariate
.fft_smooth <- function(x, kernel, device = "cuda") {
  # x:      (nrows x ncols) tensor, already on `device`
  # kernel: (k x k)         tensor, already on `device`
  # Returns a (nrows x ncols) tensor with NA cells restored.
  
  nrows  <- x$size(1L)
  ncols  <- x$size(2L)
  
  # kernel size
  k_rows <- kernel$size(1L)
  k_cols <- kernel$size(2L)
  
  # mask NAs before FFT (replace with 0 so they don't contaminate the transform)
  na_mask <- x$isnan()
  # x       <- torch_where(na_mask, torch_zeros_like(x), x)
  x_filled <- x$nan_to_num(0)
  valid    <- na_mask$logical_not()$to(dtype = x$dtype)
  
  pad_rows <- nrows + k_rows - 1L
  pad_cols <- ncols + k_cols - 1L
  
  # zero-pad to avoid circular aliasing
  pad_k <- torch_zeros(
    c(pad_rows, pad_cols),
    device = device, dtype = torch_float())
  
  fft_conv <- function(z) {
    
    # zero-pad to avoid circular aliasing
    pad_z <- torch_zeros(
      c(pad_rows, pad_cols),
      device = device, dtype = torch_float())
    
    pad_z[1:nrows, 1:ncols] <- z
    pad_k[1:k_rows, 1:k_cols] <- kernel
    
    # 2-D FFT via two sequential 1-D FFTs (torch_fft_fft2 is the preferred
    # torch API but sequential 1-D calls are equally correct and widely supported)
    fft_z <- torch_fft_fft(torch_fft_fft(pad_z, dim = 2L), dim = 1L)
    fft_k <- torch_fft_fft(torch_fft_fft(pad_k, dim = 2L), dim = 1L)
    # multiply in frequency domain → convolve in spatial domain
    out <- torch_real(
      torch_fft_ifft(torch_fft_ifft(fft_z * fft_k, dim = 1L), dim = 2L)
    )
    # crop to original size (kernel is centred, so offset by half-kernel)
    row_off <- floor(k_rows / 2L)
    col_off <- floor(k_cols / 2L)
    
    out[
      (row_off + 1L):(row_off + nrows),
      (col_off + 1L):(col_off + ncols)
    ]
  }
  
  num <- fft_conv(x_filled)
  den <- fft_conv(valid)
  
  torch_where(den$gt(0),
              num / den,
              torch_full_like(num, NaN))
}


# alpha distance conversions
# Kernel weight / distance conversion functions
# =============================================================================
# Every supported kernel satisfies  d = u / alpha , where the DIMENSIONLESS
# u = alpha * d depends only on the kernel shape and the threshold definition.
# `.kernel_ud()` computes u; alpha_to_dist() and dist_to_alpha() are thin
# wrappers around it, which guarantees they remain exact mutual inverses.
#
# thresh_type controls what 'thresh' represents:
#   "weight" - kernel weight at the distance
#   "cdf1"   - 1D cumulative mass from 0 to d
#              (integral of kernel from 0 to d, normalised)
#   "cdf2"   - 2D radial cumulative mass from origin to d
#              (integral of 2*pi*r*kernel(r) dr, normalised)
# with p = 1 - thresh the mass INSIDE the distance.
# =============================================================================

# error function / inverse error function (via the normal CDF)
.erf    <- function(x) 2 * stats::pnorm(x * sqrt(2)) - 1
.erfinv <- function(p) qnorm((1 + p) / 2) / sqrt(2)

# .kernel_ud --------------------------------------------------------------- ####
# Dimensionless u = alpha * d for a given shape / threshold definition.
#
# Kernels (R = 1/alpha is the support radius for thresh / lin / epan):
#   exp    k = exp(-a r)             weight & cdf1: u = -log(thresh)
#                                    cdf2: 1 - e^-u (1 + u) = p           [root]
#   gaus   k = exp(-r^2 a^2 / 2)     weight: u = sqrt(-2 log thresh)
#                                    cdf1:   u = sqrt(2) * erfinv(p)
#                                    cdf2:   u = sqrt(-2 log(1 - p))
#   thresh k = 1 on [0, R]           weight: u = 1
#                                    cdf1:   u = p          (mass ~ d)
#                                    cdf2:   u = sqrt(p)    (mass ~ d^2)
#   lin    k = 1 - a r               weight: u = 1 - thresh
#                                    cdf1:   u = 1 - sqrt(1 - p)
#                                    cdf2:   3u^2 - 2u^3 = p              [root]
#   epan   k = (3/4)(1 - (a r)^2)    weight: u = sqrt(1 - 4*thresh/3)
#                                    cdf1:   u^3 - 3u + 2p = 0            [root]
#                                    cdf2:   u = sqrt(1 - sqrt(1 - p))
#   inv    k = 1/(a r + 1)           not integrable -> CDFs undefined
#   invsq  k = 1/(a^2 r^2 + 1)       cdf1: u = tan(p*pi/2); cdf2 divergent
.kernel_ud <- function(shape, thresh = 0.05,
                       thresh_type = c("weight", "cdf1", "cdf2")) {
  thresh_type <- match.arg(thresh_type)
  shape <- .validate_kernel_shape(shape)
  
  # kernels whose required integral diverges -> fall back to the weight rule
  if (thresh_type != "weight" &&
      (shape == "inv" || (shape == "invsq" && thresh_type == "cdf2"))) {
    cli::cli_warn(c(
      "CDF-based {.arg thresh_type} is undefined for the {.val {shape}} kernel \\
       (the required integral diverges).",
      "i" = "Falling back to {.code thresh_type = 'weight'}."
    ))
    thresh_type <- "weight"
  }
  
  p <- 1 - thresh
  # monotone root finder on [lo, hi], vectorised over p
  root <- function(f, lo, hi) vapply(seq_along(p), function(i)
    stats::uniroot(function(u) f(u, p[i]), lower = lo, upper = hi,
                   tol = .Machine$double.eps^0.5)$root, numeric(1))
  
  if (thresh_type == "weight") {
    if (shape == "epan" && any(thresh > 3/4))
      cli::cli_abort("{.arg thresh} must be <= 3/4 for the epan kernel (maximum weight is 3/4).")
    return(switch(shape,
                  exp    = -log(thresh),
                  gaus   = sqrt(-2 * log(thresh)),
                  thresh = ifelse(thresh == 1, 0, 1),
                  lin    = 1 - thresh,
                  inv    = 1 / thresh - 1,
                  invsq  = sqrt(1 / thresh - 1),
                  epan   = sqrt(1 - 4/3 * thresh)))
  }
  
  if (thresh_type == "cdf1") {
    return(switch(shape,
                  exp    = -log(1 - p),                 # identical to the weight rule
                  gaus   = sqrt(2) * .erfinv(p),
                  thresh = p,
                  lin    = 1 - sqrt(1 - p),
                  invsq  = tan(p * pi / 2),
                  epan   = root(function(u, pp) u^3 - 3 * u + 2 * pp, 0, 1)))
  }
  
  # cdf2
  switch(shape,
         exp    = root(function(u, pp) 1 - exp(-u) * (1 + u) - pp, 1e-10, 1e3),
         gaus   = sqrt(-2 * log(1 - p)),
         thresh = sqrt(p),
         lin    = root(function(u, pp) 3 * u^2 - 2 * u^3 - pp, 0, 1),
         epan   = sqrt(1 - sqrt(1 - p)))
}

# .kernel_cdf -------------------------------------------------------------- ####
# FORWARD map: cumulative mass fraction contained within u = alpha * d.
# (the exact inverse of the cdf branches of .kernel_ud; used by d_a_t()).
.kernel_cdf <- function(u, shape, thresh_type = c("cdf1", "cdf2")) {
  thresh_type <- match.arg(thresh_type)
  shape <- .validate_kernel_shape(shape)
  u <- pmax(u, 0)
  if (thresh_type == "cdf1") {
    switch(shape,
           exp    = 1 - exp(-u),
           gaus   = .erf(u / sqrt(2)),
           thresh = pmin(u, 1),
           lin    = pmin(2 * u - u^2, 1),
           invsq  = atan(u) / (pi / 2),
           epan   = pmin((3 * u - u^3) / 2, 1),
           inv    = NA_real_)
  } else {
    switch(shape,
           exp    = 1 - exp(-u) * (1 + u),
           gaus   = 1 - exp(-u^2 / 2),
           thresh = pmin(u^2, 1),
           lin    = pmin(3 * u^2 - 2 * u^3, 1),
           epan   = pmin(2 * u^2 - u^4, 1),
           inv    = NA_real_,
           invsq  = NA_real_)
  }
}

# back-compat: kept because older code referenced it directly
.exp_cdf2_inv <- function(p, alpha) .kernel_ud("exp", 1 - p, "cdf2") / alpha

# alpha_to_dist ------------------------------------------------------------ ####
alpha_to_dist <- function(object, shape = c("exp", "gaus", "thresh", "lin", "inv", "invsq", "epan"),
                          scale = c("natural", "working"),
                          thresh = 0.05, thresh_type = c("weight", "cdf1", "cdf2")) {
  thresh_type <- rlang::arg_match(thresh_type)
  scale <- rlang::arg_match(scale)
  
  # get alphas from a fitted model, else treat object as an alpha vector
  if (inherits(object, "cshs_fit")) {
    alpha <- get_alpha(object)
    shape <- get_shape(object)
  } else {
    alpha <- object
  }
  if (scale == "working") alpha <- softplus(alpha)
  
  # u depends only on shape + threshold definition; supports one shape per alpha
  u <- if (length(shape) > 1) {
    vapply(shape, function(s) as.numeric(.kernel_ud(s, thresh, thresh_type)), numeric(1))
  } else {
    .kernel_ud(shape, thresh, thresh_type)
  }
  unname(u / alpha)
}

# dist_to_alpha ------------------------------------------------------------ ####
dist_to_alpha <- function(dist, shape = c("exp", "gaus", "thresh", "lin", "inv", "invsq", "epan"),
                          scale = c("natural", "working"),
                          thresh = 0.05, thresh_type = c("weight", "cdf1", "cdf2")) {
  thresh_type <- rlang::arg_match(thresh_type)
  scale <- rlang::arg_match(scale)
  
  u <- if (length(shape) > 1) {
    vapply(shape, function(s) as.numeric(.kernel_ud(s, thresh, thresh_type)), numeric(1))
  } else {
    .kernel_ud(shape, thresh, thresh_type)
  }
  alpha <- unname(u / dist)
  
  if (scale == "working") alpha <- softplus_inv(alpha)
  alpha
}

# validate supported kernels ---------------------------------------------- ####
.validate_kernel_shape <- function(shape,
                                   choices = c("exp", "gaus", "thresh", "lin", "inv", "invsq", "epan"),
                                   several.ok = FALSE) {
  # replicate match.arg behaviour: if shape is the full choices vector,
  # the caller didn't supply it — return the first element as default
  if (identical(shape, choices)) return(invisible(choices[1L]))
  
  if (!several.ok && length(shape) != 1L) {
    cli::cli_abort(c(
      "{.arg shape} must be a single string.",
      "x" = "Got a vector of length {length(shape)}."
    ))
  }
  
  matched <- charmatch(shape, choices)
  
  if (any(is.na(matched)) || any(matched == 0L)) {
    bad <- shape[is.na(matched) | matched == 0L]
    cli::cli_abort(c(
      "{.arg shape} must be one of {.or {.val {choices}}}.",
      "x" = "Got {.val {bad}}."
    ))
  }
  
  invisible(choices[matched])
}