nodes_order <- function(graph) {
  nodes <- graph %>%
    tidygraph::activate(nodes) %>%
    as.data.frame() %>% {
      1:nrow(.)
    }
  edges <- graph %>%
    tidygraph::activate(edges) %>%
    as.data.frame()

  edges <- edges %>%
    dplyr::mutate(source = !from %in% to)

  unique(c(edges[edges$source, "from"], edges[!edges$source, "from"], nodes))
}

simulate_schema_obj <- function(schema_obj) {

  schema_obj$get_schema("graph") %>%
    tidygraph::mutate(
      state = tidygraph::map_bfs(
        nodes_order(schema_obj$get_schema("graph")), mode = "in",
        .f = simulate_table, schema_obj = schema_obj
      )
    )
  return(invisible(TRUE))
}

simulate_table <- function(node, schema_obj, ...) {

  schema_nodes <- schema_obj$get_schema("graph")
  tbl_name <- tibble::as_tibble(schema_nodes)[node, "name", drop = TRUE]
  table <- schema_obj$get_table(tbl_name, "graph")

  verbmsg(glue::glue("Simulating table {sQuote(tbl_name)} started.."))

  table %>%
    tidygraph::mutate(
      state = tidygraph::map_bfs(
        nodes_order(table), mode = "in", .f = generate_column,
        table_name = tbl_name, schema_obj = schema_obj
      )
    )

  return(invisible(TRUE))
}

generate_column <- function(node, table_name, schema_obj, ...) {

  table_nodes <- schema_obj$get_table(table_name, "graph")
  col_name <- tibble::as_tibble(table_nodes)[node, "name", drop = TRUE]
  col_def <- schema_obj$get_column(col_name, table_name)

  verbmsg(glue::glue("Simulating column {sQuote(col_name)} started.."), 2)

  n <- attr(schema_obj$get_schema(), "schema-nrows")[[table_name]]
  faked_col <- fake_column(n, col_def, schema_obj$get_schema(), schema_obj$get_opts())

  schema_obj$update_table_col(col_name, table_name, faked_col)

  return(invisible(TRUE))
}

get_simulated_tbl <- function(table_name, schema) {
  cols_order <- names(schema[[get_schema_name(schema)]]$tables[[table_name]]$columns)
  simulated_table <- attr(schema, "schema-graph") %>%
    tidygraph::activate(nodes) %>%
    tidygraph::filter(name == !!table_name) %>%
    tidygraph::pull(table) %>%
    dplyr::first()
  simulated_table %>% dplyr::select(dplyr::any_of(!!cols_order))
}
