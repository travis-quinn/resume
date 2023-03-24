
create_skills_heatmap <- function(skills_data) {

  exp_level_levels <- c(5, 4, 3, 2, 1, 0, NA)
  exp_level_descriptions <- c(
    "5 - Teach a course on it",
    "4 - Do advanced tasks with minimal Googling",
    "3 - Knows what to Google to be proficient",
    "2 - Used, with guidance or niche topics",
    "1 - Not sure how, but it works",
    "0 - Have not used",
    "NA - Software not applicable to skill"
  )

  # 5 #08519C --- #ffffff
  # 4 #3182BD --- #cccccc
  # 3 #6BAED6 --- #666666
  # 2 #9ECAE1 --- #4c4c4c
  # 1 #C6DBEF --- #000000

  text_label_colors <- c(
    "5" = "#ffffff",
    "4" = "#ffffff",
    "3" = "#000000",
    "2" = "#000000",
    "1" = "#000000",
    "0" = "#000000"
  )

  ggplot2::ggplot(skills_data, ggplot2::aes(x = Skill, y = Software, fill = Level)) +
    ggplot2::geom_tile(color = "white",
                       lwd = 0.3,
                       linetype = 1) +
    ggplot2::geom_text(ggplot2::aes(label = Level, color = Level),
                       size = 3,
                       fontface = "bold",
                       na.rm = TRUE) +
    ggplot2::scale_color_manual(values = text_label_colors) +
    #ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(type = "seq",
                               palette = "Blues",
                               direction = -1,
                               na.value = "gray",
                               name = "Experience Levels",
                               breaks = exp_level_levels,
                               labels = exp_level_descriptions) +
    ggplot2::scale_x_discrete(position = "top",
                              # guide = ggplot2::guide_axis(n.dodge = 2),
                              labels = scales::label_wrap(10),
                              expand = c(0, 0)) +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(10),
                              expand = c(0, 0)) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.spacing.y = ggplot2::unit(0, "cm"),
      legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 3)),
      legend.margin = ggplot2::margin(t = -5),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.justification = "left",
      legend.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      legend.box.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      axis.ticks.length = ggplot2::unit(0, "cm"),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 10,
                                   family = "sans"),
      # axis.title.y = ggplot2::element_text(
      #   size = 10,
      #   face = "bold",
      #   margin = ggplot2::margin(l = 0, r = -30,
      #                            t = -70, b = 0),
      #   hjust = 1,
      #   angle = 0
      # )),
      axis.title.y = ggplot2::element_blank(),
      # axis.title.x = ggplot2::element_text(
      #   size = 10,
      #   face = "bold"
      # )
      axis.text.x.top = ggplot2::element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      # it needed to be axis.text.x.TOP - we were missing the TOP part because
      #   vjust acts differently if the axis text is on top rather than the bottom
      ###^ :)
      axis.title.x = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'transparent'),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(direction = "vertical",
                                                 byrow = TRUE),
                    color = "none")

}



save_skills_heatmap <- function(p, path) {

  ggplot2::ggsave(plot = p, filename = path,
                  bg = "transparent",
                  device = "svg",
                  width = 370 / 96,
                  height = 600 / 96,
                  units = "in")

}
