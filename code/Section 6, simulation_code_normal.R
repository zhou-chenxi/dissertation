library(LogConcaveDESM, warn.conflicts = FALSE)
library(patchwork)
library(ggplot2)

# ==================================================================================================
# normal distribution
set.seed(1)
N <- 100
domain <- c(-Inf, Inf)
plot_xlim <- c(-4, 4)
plot_ylim <- c(-0.01, 0.59)
data_normal <- rnorm(N, 0, 1)

binwidth <- 2 * stats::IQR(data_normal) / N ** (1/3)
hist_plot <- ggplot() + geom_histogram(
    data = data.frame(x = data_normal),
    aes(x = x, y = ..density..),
    binwidth = binwidth,
    color = "forestgreen",
    fill = "forestgreen",
    alpha = 0.1) +
    coord_cartesian(xlim = plot_xlim, ylim = plot_ylim) + 
    ggplot2::geom_rug(
        data = data.frame(original_data = data_normal),
        ggplot2::aes(x = original_data),
        color = 'black',
        alpha = 0.5
    ) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.text = element_text(size = 14),
                   axis.title = element_text(size = 16)) 
hist_plot


# plot of ML only 
mle <- logcondens::logConDens(data_normal)
plot_points_cnt <- 500
newxs <- seq(min(data_normal), max(data_normal), length.out = plot_points_cnt)
mle_result <- as.data.frame(logcondens::evaluateLogConDens(newxs, res = mle, which = 1:3)[, c('xs', 'density')])
newx2 <- seq(plot_xlim[1], min(data_normal) - 1e-10, length.out = plot_points_cnt)
newx3 <- seq(max(data_normal) + 1e-10, plot_xlim[2], length.out = plot_points_cnt)
colnames(mle_result) <- c('newx', 'mle')
mle_result_else2 <- data.frame(newx = newx2, mle = 0)
mle_result_else3 <- data.frame(newx = newx3, mle = 0)

mle_hist_plot <- hist_plot +
    ggplot2::geom_line(
        data = mle_result,
        ggplot2::aes(x = newx, y = mle),
        color = 'red', 
        size = 2) + 
    ggplot2::scale_color_manual(
        # labels = c("ML", "SM", "True"),
        values = c("red")) + 
    ggplot2::labs(
        x = 'x',
        y = 'density') + ggplot2::theme_bw() 
mle_hist_plot

mle_hist_plot <- mle_hist_plot + 
    ggplot2::geom_line(
    data = mle_result_else2,
    ggplot2::aes(x = newx, y = mle),
    color = 'red', 
    size = 2)  + 
    ggplot2::geom_line(
        data = mle_result_else3,
        ggplot2::aes(x = newx, y = mle),
        color = 'red', 
        size = 2) + 
    ggplot2::scale_color_manual(
        # labels = c("ML", "SM", "True"),
        values = c("red"))
mle_hist_plot


mle_hist_plot <- mle_hist_plot + 
    ggplot2::geom_segment(ggplot2::aes(x = max(data_normal), y = 0, 
                                       xend = max(data_normal), yend = mle_result$mle[length(mle_result$mle)]), 
                          color = "red", linetype = "dashed", size = 0.5) + 
    ggplot2::geom_segment(ggplot2::aes(x = min(data_normal), y = 0, 
                                       xend = min(data_normal), yend = mle_result$mle[1]), 
                          color = "red", linetype = "dashed", size = 0.5)
mle_hist_plot

true_plot_x <- seq(plot_xlim[1], plot_xlim[2], length.out = plot_points_cnt)
true_density <- dnorm(true_plot_x, 0, 1)
true_density_df <- data.frame(plotx = true_plot_x, ploty = true_density)

mle_hist_plot <- mle_hist_plot + # geom_line(data = true_density_df, aes(x = plotx, y = ploty), color = 'black', size = 2) + 
    ggplot2::theme(axis.text = element_text(size = 20),
                   axis.title = element_text(size = 20), 
                   legend.text = element_text(size = 20)) 
mle_hist_plot

# computation of best SM log-concave density estimate
lambda_cand <- exp(seq(-10, 1, by = 0.5)) 
opt_den <- cv_optimal_density_estimate(
    data = data_normal,
    domain = domain,
    penalty_param_candidates = lambda_cand,
    fold_number = 5
)
# optimal penalty parameter is 0.22313016014843, whose log is -1.5

opt_den <- lcd_scorematching(
    data = data_normal, 
    domain = domain, 
    penalty_param = exp(-2.0), 
    verbose = FALSE
)

plot_ld2 <- plot_logdensity_deriv2(
    scorematching_logconcave = opt_den,
    plot_domain = plot_xlim
)

plot_ld1 <- plot_logdensity_deriv1(
    scorematching_logconcave = opt_den,
    plot_domain = plot_xlim
)

plot_ld <- plot_logdensity(
    scorematching_logconcave = opt_den,
    plot_domain = plot_xlim
)

plot_den <- plot_density(
    scorematching_logconcave = opt_den,
    plot_domain = plot_xlim,
    plot_points_cnt = 500,
    plot_hist = TRUE
)

plot_ld2 + plot_ld1 + plot_ld + plot_den

plot_both <- plot_mle_scorematching(
    scorematching_logconcave = opt_den,
    plot_domain = plot_xlim,
    plot_points_cnt = 500,
    plot_hist = TRUE)

# plot with the true density function
plot_x <- seq(plot_xlim[1], plot_xlim[2], length.out = 500)
plot_y <- dnorm(plot_x, 0, 1)
plot_true_data <- data.frame(x = plot_x, y = plot_y)
plot_both + geom_line(data = plot_true_data, mapping = aes(x = x, y = y), color = 'black', size = 1.0)

merged_result <- mle_result
den_vals <- evaluate_density(
    scorematching_logconcave = opt_den, 
    newx = seq(plot_xlim[1], plot_xlim[2], length.out = plot_points_cnt)
)

merged_result$sm <- den_vals$density_vals
merged_result$true <- dnorm(seq(plot_xlim[1], plot_xlim[2], length.out = plot_points_cnt), 0, 1)

gather_merged_result <- tidyr::gather(
    merged_result,
    key = 'key',
    value = 'value',
    -newx)

two_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
        data = gather_merged_result,
        ggplot2::aes(x = newx, y = value, color = key),
        size = 1.5) +
    ggplot2::scale_color_manual(
        labels = c("ML", "SM", "True"),
        values = c("red", "dodgerblue4", "black")) +
    ggplot2::labs(
        x = 'x',
        y = 'density') +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::xlim(plot_xlim) + 
    ggplot2::ylim(plot_ylim) + 
    ggplot2::theme(axis.text = element_text(size = 14),
                   axis.title = element_text(size = 16), 
                   legend.text = element_text(size = 10)) 

hist_binwidth <- 2 * stats::IQR(data_normal) / (N ** (1/3))

two_plot <- two_plot + ggplot2::geom_histogram(
    data = data.frame(x = data_normal),
    ggplot2::aes(x = x, y = ..density..),
    binwidth = hist_binwidth,
    color = "forestgreen",
    fill = "forestgreen",
    alpha = 0.1) + 
    ggplot2::geom_rug(
        data = data.frame(original_data = data_normal),
        ggplot2::aes(x = original_data),
        color = 'black',
        alpha = 0.5
    ) 
two_plot

two_plot <- two_plot + 
    ggplot2::geom_line(
        data = mle_result_else2,
        ggplot2::aes(x = newx, y = mle),
        color = 'red', 
        size = 2)  + 
    ggplot2::geom_line(
        data = mle_result_else3,
        ggplot2::aes(x = newx, y = mle),
        color = 'red', 
        size = 2) + 
    ggplot2::scale_color_manual(
        labels = c("ML", "SM", "True"),
        values = c("red", "dodgerblue4", "black"))
two_plot


two_plot <- two_plot + 
    ggplot2::geom_segment(ggplot2::aes(x = max(data_normal), y = 0, 
                                       xend = max(data_normal), yend = mle_result$mle[length(mle_result$mle)]), 
                          color = "black", linetype = "dashed", size = 1) + 
    ggplot2::geom_segment(ggplot2::aes(x = min(data_normal), y = 0, 
                                       xend = min(data_normal), yend = mle_result$mle[1]), 
                          color = "black", linetype = "dashed", size = 1)
two_plot
