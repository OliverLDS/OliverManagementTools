OliverOrganizationalChart <- function(df) {
  n_level <- max(df$level)
  median_bottom <- median(df %>% filter(level==n_level) %>% .$id)

  df <- df %>%
    mutate(y = 2-2*level) %>%
    mutate(
      x=case_when(
        level==1~0,
        level==n_level~3*(id-median_bottom),
        T~-Inf
      )
    )

  for (l in (n_level-1):2) {
    for (i in df %>% filter(level==l) %>% .$id) {
      df <- df %>%
        mutate(
          x=if_else(level==l&id==i, df %>% filter(level==l+1 & parent_id==i) %>% .$x %>% mean(), x)
        )
    }
  }

  p <- df %>% ggplot() +
    geom_rect(aes(xmin=x-1, xmax=x+1, ymin=y-0.5, ymax=y+0.5),
              color='black', fill='white') +
    geom_text(aes(x=x, y=y, label=label))

  df_h <- df %>% group_by(level, parent_id) %>%
    summarise(xmin=min(x), xmax=max(x)) %>%
    filter(level>1) %>%
    mutate(y=3-2*level)

  p <- p + geom_segment(data = df_h,
                        aes(x = xmin, xend = xmax, y = y, yend = y))

  df_u <- df %>% filter(level>1)

  p <- p + geom_segment(data = df_u,
                        aes(x = x, xend = x, y = y + 0.5, yend = y + 1))

  df_d <- df %>% filter(level<n_level)

  p <- p + geom_segment(data = df_d,
                        aes(x = x, xend = x, y = y - 0.5, yend = y - 1))

  p <- p + theme_void(base_family = 'Times New Roman')

  return(p)
}
