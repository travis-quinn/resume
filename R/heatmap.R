
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

  ggplot2::ggplot(skills_data, ggplot2::aes(x = Skill, y = Software, fill = Level)) +
    ggplot2::geom_tile(color = "white",
                       lwd = 1,
                       linetype = 1) +
    ggplot2::geom_text(ggplot2::aes(label = Level), size = 3, na.rm = TRUE) +
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
                              guide = ggplot2::guide_axis(n.dodge = 2),
                              labels = scales::label_wrap(10),
                              expand = c(0, 0)) +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(10),
                              expand = c(0, 0)) +
    ggplot2::theme(legend.position = "bottom",
                   axis.ticks.length = ggplot2::unit(0, "cm"),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 9),
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
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(direction = "vertical"))

}



save_skills_heatmap <- function(p, path) {

  ggplot2::ggsave(plot = p, filename = path,
                  bg = "white",
                  device = "svg",
                  width = 0.9 * 0.4 * 8.5,
                  height = 6.5,
                  units = "in")

}
