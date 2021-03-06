{
    application,
    advent_of_code,
    [
        {
            description,
            "Advent of Code 2021"
        },
        {
            vsn,
            "1"
        },
        {
            modules,
            [
                advent_of_code,
                day_supervisor,
                import,
                log_handler,
                main_supervisor,
                parts,
                input_transform,
                part1,
                part2
            ]
        },
        {
            registered,
            [
                aoc_main_supervisor,
                aoc_day_supervisor,
                aoc_logger_manager,
                aoc_input_transform,
                aoc_data_server_0,
                aoc_data_server_1
            ]
        },
        {
            applications,
            []
        },
        {
            mod,
            {
                advent_of_code,
                [input_transform, part1, part2]
            }
        }
    ]
}.