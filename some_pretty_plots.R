library(tidyverse)

sp500_data_wide <- gt::sp500 |>
  select(date, open, close) |> 
  filter(year(date) == 2014, month(date) == 1)
sp500_data_wide


sp500_data <- sp500_data_wide |> 
  pivot_longer(
    cols = -date,
    names_to = 'type',
    values_to = 'price'
  )

  sp500_data |> 
    ggplot(aes(date, price, col = type)) +
    geom_line(linewidth = 1.25)


  sp500_data |> 
    ggplot(aes(date, price, col = type)) +
    geom_line(linewidth = 1.25) +
    theme_minimal(
      base_size = 20, 
      base_family = 'Source Sans Pro'
    ) +
    theme(
      panel.grid.minor = element_blank()
    )


    sp500_data |> 
      ggplot(aes(date, price, col = type)) +
      geom_line(linewidth = 1.25) +
      theme_minimal(
        base_size = 20, 
        base_family = 'Source Sans Pro'
      ) +
      theme(
        panel.grid.minor = element_blank()
      ) +
      scale_color_manual(
        values = c('#0072B2', '#D55E00')
      ) 

      sp500_data |> 
        ggplot(aes(date, price, col = type)) +
        geom_line(linewidth = 1.25) +
        theme_minimal(
          base_size = 20, 
          # base_family = 'Source Sans Pro'
        ) +
        theme(
          panel.grid.minor = element_blank()
        ) +
        scale_color_manual(
          values = c('#0072B2', '#D55E00')
        ) +
        labs(
          x = element_blank(), 
          y = element_blank(),
          title = 'SP500 Prices in January 2014'
        ) 

        sp500_data |> 
          ggplot(aes(date, price, col = type)) +
          geom_line(linewidth = 1.25) +
          theme_minimal(
            base_size = 20, 
            # base_family = 'Source Sans Pro'
          ) +
          theme(
            panel.grid.minor = element_blank()
          ) +
          scale_color_manual(
            values = c('#0072B2', '#D55E00')
          ) +
          labs(
            x = element_blank(), 
            y = element_blank(),
            title = 'SP500 Prices in January 2014'
          ) +
          scale_y_continuous(
            labels = scales::label_dollar()
          )

          sp500_data |> 
            ggplot(aes(date, price, col = type)) +
            geom_line(linewidth = 1.25) +
            geom_text(
              data = sp500_data |> slice_head(n = 1, by = type),
              aes(label = type),
              hjust = 0,
              vjust = 0,
              # family = 'Source Sans Pro',
              size = 10,
              nudge_x = 0.1
            ) +
            theme_minimal(
              base_size = 20, 
              # base_family = 'Source Sans Pro'
            ) +
            theme(
              panel.grid.minor = element_blank(),
              legend.position = 'none'
            ) +
            scale_color_manual(
              values = c('#0072B2', '#D55E00')
            ) +
            labs(
              x = element_blank(), 
              y = element_blank(),
              title = 'SP500 Prices in January 2014'
            ) +
            scale_y_continuous(
              labels = scales::label_dollar()
            ) +
            scale_x_date(
              limits = c(
                make_date(2014, 1, 1), 
                make_date(2014, 2 ,3)
              )
            )


            sp500_data_with_nicer_labels <- sp500_data |> 
              mutate(
                type = if_else(
                  type == 'open',
                  'Opening price',
                  'Closing price'
                )
              ) 
            
sp500_data_with_nicer_labels|> 
              ggplot(aes(date, price, col = type)) +
              geom_line(linewidth = 1.25) +
              geom_text(
                data = sp500_data_with_nicer_labels |> 
                  slice_head(n = 1, by = type),
                aes(label = type),
                hjust = 0,
                vjust = 0,
                family = 'Source Sans Pro',
                size = 10,
                nudge_x = 0.1
              ) +
              theme_minimal(
                base_size = 20, 
                base_family = 'Source Sans Pro'
              ) +
              theme(
                panel.grid.minor = element_blank(),
                legend.position = 'none'
              ) +
              scale_color_manual(
                values = c('#0072B2', '#D55E00')
              ) +
              labs(
                x = element_blank(), 
                y = element_blank(),
                title = 'SP500 Prices in January 2014'
              ) +
              scale_y_continuous(
                labels = scales::label_dollar()
              ) +
              scale_x_date(
                limits = c(
                  make_date(2014, 1, 1), 
                  make_date(2014, 2, 8)
                )
              )

sp500_data_with_nicer_labels |> 
  ggplot(aes(date, price, col = type)) +
  ggbraid::geom_braid(
    data = sp500_data_wide, 
    aes(
      y = NULL, ## Overwrite the inherited aes from ggplot()
      col = NULL, 
      ymin = open, 
      ymax = close, 
      fill = open < close
    ), 
    alpha = 0.6
  ) +
  geom_line(linewidth = 1.25) +
  geomtextpath::geom_textline(
    data = sp500_data_with_nicer_labels |> 
      filter(type == 'Opening price'),
    aes(label = type),
    hjust = 0.76,
    vjust = 0,
    # family = 'Source Sans Pro',
    size = 8
  ) +
  geomtextpath::geom_textline(
    data = sp500_data_with_nicer_labels |> 
      filter(type == 'Closing price'),
    aes(label = type),
    hjust = 0.77,
    vjust = 1,
    # family = 'Source Sans Pro',
    size = 8,
    text_smoothing = 40,
    offset = unit(-14, 'mm')
  ) +
  ggforce::geom_mark_circle(
    data = tibble(
      date = make_date(2014, 1, 24),
      price = 1820
    ),
    aes(
      col = NULL, 
      label = 'This area signals whether the\nclosing price or opening price was\nhigher on a given day'
    ),
    fill = 'white',
    color = 'grey20',
    alpha = 1,
    x0 = make_date(2014, 1, 13),
    y0 = 1805,
    # label.family = 'Source Sans Pro',
    label.colour = 'grey20',
    label.hjust = 0,
    label.fontsize = 12,
    label.fontface = 'plain',
    con.colour = 'grey20',
    con.cap = unit(1, 'mm'),
    expand = 0.011
  ) +
  theme_minimal(
    base_size = 20, 
    # base_family = 'Source Sans Pro'
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'none'
  ) +
  scale_color_manual(
    values = c('#0072B2', '#D55E00')
  ) +
  scale_fill_manual(
    values = c('TRUE' = '#0072B2', 'FALSE' = '#D55E00')
  ) +
  labs(
    x = element_blank(), 
    y = element_blank(),
    title = 'SP500 Prices in January 2014'
  ) +
  scale_y_continuous(
    labels = scales::label_dollar()
  ) 
## `geom_braid()` using method = 'line'