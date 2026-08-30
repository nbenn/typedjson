# Captured from blockr.core 0.1.4 with blockr_ser() on a board of two blocks,
# one link and one stack. Frozen as a literal so the suite carries a payload
# that was really produced by the code this format is meant to persist.

corpus_board <- list(object = "board", payload = list(blocks = list(object = c("blocks",
"vctrs_vctr", "list"), payload = list(a = list(object = c("dataset_block",
"data_block", "block", "vctrs_vctr", "list"), payload = list(
    dataset = "BOD", package = "datasets", block_name = "Dataset"),
    constructor = list(object = "blockr_ctor", constructor = "new_dataset_block",
        package = "blockr.core", version = "0.1.4")), m = list(
    object = c("merge_block", "transform_block", "block", "vctrs_vctr",
    "list"), payload = list(by = "Time", all_x = FALSE, all_y = FALSE,
        block_name = "Merge"), constructor = list(object = "blockr_ctor",
        constructor = "new_merge_block", package = "blockr.core",
        version = "0.1.4")))), links = list(object = c("links",
"vctrs_rcrd", "vctrs_vctr"), payload = list(ab = list(object = c("link",
"vctrs_vctr", "list"), payload = list(from = "a", to = "m", input = "x"),
    constructor = list(object = "blockr_ctor", constructor = "new_link",
        package = "blockr.core", version = "0.1.4")))), stacks = list(
    object = c("stacks", "vctrs_vctr", "list"), payload = list(
        s = list(object = "stack", payload = list(blocks = "a",
            name = "Stack"), constructor = list(object = "blockr_ctor",
            constructor = "new_stack", package = "blockr.core",
            version = "0.1.4")))), options = list(object = c("board_options",
"vctrs_vctr", "list"), payload = list(list(object = c("board_name_option",
"board_option"), payload = list(category = "Board options"),
    constructor = list(object = "blockr_ctor", constructor = "new_board_name_option",
        package = "blockr.core", version = "0.1.4")), list(object = c("show_conditions_option",
"board_option"), payload = list(value = c("warning", "error"),
    category = "Board options"), constructor = list(object = "blockr_ctor",
    constructor = "new_show_conditions_option", package = "blockr.core",
    version = "0.1.4")), list(object = c("thematic_option", "board_option"
), payload = list(category = "Theme options"), constructor = list(
    object = "blockr_ctor", constructor = "new_thematic_option",
    package = "blockr.core", version = "0.1.4")), list(object = c("dark_mode_option",
"board_option"), payload = list(category = "Theme options"),
    constructor = list(object = "blockr_ctor", constructor = "new_dark_mode_option",
        package = "blockr.core", version = "0.1.4"))))), constructor = list(
    object = "blockr_ctor", constructor = "new_board", package = "blockr.core",
    version = "0.1.4"), version = "0.1.4", id = NULL)
