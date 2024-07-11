barplot <- function(df, variable, ylab) {

    # Calculate visually pleasing y-axis limits w. padding for p-values
    y_values <- df %>% dplyr::filter(treatment != "pla") %>% dplyr::pull(!!rlang::sym(variable))

    y_breaks <- scales::pretty_breaks(n = 5)(y_values)

    y_min <- min(y_breaks)
    y_max <- max(y_breaks * 1.25)

    # Plot figure
    fig <- df %>%
        dplyr::filter(treatment != "pla") %>%
        ggplot(aes(x = treatment, y = !!rlang::sym(variable), color = "black", fill = treatment)) +
        geom_bar(stat = "summary", fun = "mean", color = NA) +
        geom_point(size = 4,
                   aes(shape = sex,
                       alpha = 0.5),
                   stroke = 0.5,
                   color = "black",
                   position = position_jitterdodge(
                       jitter.width = 0.2,
                       dodge.width = 0.5)
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
        theme(
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.25),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "white"),
            axis.line = element_blank(),
            axis.text = element_text(color = "black"),
            axis.title.y = element_text(size = 11),
            text = element_text(size = 11, family = "Source Sans Pro", color = "black"),
            legend.title = element_blank(),
            legend.key.size = (grid::unit(5, "mm")),
            legend.key.height = (grid::unit(5, "mm")),
            legend.position = "top",
            legend.justification = "left",
            legend.margin = margin(t = 0, r = 0, b = -10, l = 0),
            title = element_text(size = 6),
            aspect.ratio = 1,
            strip.background = element_blank()
        ) +
        scale_color_manual(
            values = c("for_i" = for_i_color,
                       "for_o" = for_o_color,
                       "sym" = sym_color,
                       man = mannitol_color)
            ) +
        scale_fill_manual(
            values = c("for_i" = for_i_color, "for_o" = for_o_color, "sym" = sym_color, man = mannitol_color),
            labels = c(for_i = "FOR inhaled (54 µg)",
                       for_o = "FOR oral (120 µg)",
                       sym = "Symbicort\n (54 µg inh. formoterol\n+1920 µg budesonide)",
                       man = "Mannitol")
        ) +
        scale_shape_manual(
            values = c("M" = 21, "W" = 22),
            labels = c("M" = "Male", "W" = "Female"),
            guide = guide_legend(override.aes = list(fill = "white", color = "black"))
        ) +
        guides(
            color = "none",
            #fill = guide_legend(override.aes = list(shape = 22, color = NA)),
            fill = "none",
            shape = guide_legend(override.aes = list(fill = "white", color = "black")),
            alpha = "none"
        ) +
        labs(x = "", y = ylab) +
        scale_x_discrete(labels = c(for_i = "FOR\ninhaled (54 µg)",
                                    for_o = "FOR\noral (120 µg)",
                                    sym = "Symbicort\n (54 µg inh. formoterol\n+1920 µg budesonide)",
                                    man = "Mannitol")) +
        scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks)

    print(fig)
}

barplot_sex <- function(df, variable, ylab) {


    #Calculate visually pleasing y-axis limits w. padding for p-values
    y_values <- df %>% filter(treatment != "pla") %>% pull(!!sym(variable))

    y_breaks <- scales::pretty_breaks(n = 5)(y_values)

    y_min <- min(y_breaks)

    y_max <- max(y_breaks*1.25)

    #Plot figure
    fig <- df %>%
        dplyr::filter(treatment != "pla") %>%
        ggplot2::ggplot(aes(x = treatment, y = !!sym(variable), group = sex))+
        geom_point(aes(color = treatment, shape = sex),
                   size = 4,
                   alpha = 0.5,
                   position = position_dodge(width = 1)
        )+
        geom_bar(aes(fill = treatment),
                 stat = "summary",
                 fun = "mean",
                 position = position_dodge(width = 1)
        )+
        geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25)+
        theme(
            panel.background = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.25),
            panel.grid.minor=element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill = "white"),
            axis.line = element_blank(),
            axis.text = element_text(color = "black"),
            #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            text = element_text(size = 8, family="Source Sans Pro", color = "black"),
            axis.title = element_text(size = 8, family="Source Sans Pro"),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.key.size = (unit(3, "mm")),
            legend.position = "right",
            title = element_text(size = 6),
            aspect.ratio = 1,
            strip.background = element_blank()
        )+
        scale_color_manual(
            values = c("for_i" = for_i_color, "for_o" = for_o_color, "sym" = sym_color, man = mannitol_color),
            labels = c(for_i = "FOR inhaled", for_o = "FOR oral", sym = "Symbicort", man = "Mannitol")
        )+
        scale_fill_manual(
            values = c("for_i" = for_i_color, "for_o" = for_o_color, "sym" = sym_color, man = mannitol_color),
            labels = c(for_i = "FOR inhaled", for_o = "FOR oral", sym = "Symbicort", man = "Mannitol")
        )+
        scale_shape_manual(
            values = c("M" = 16, "W" = 15),
            labels = c("M" = "Males", "W" = "Females")
        )+
        labs(x = "", y = ylab)+
        scale_x_discrete(labels = c(for_i = "FOR\ninhaled", for_o = "FOR\noral", sym = "Symbicort", man = "Mannitol"))

    print(fig)

}
