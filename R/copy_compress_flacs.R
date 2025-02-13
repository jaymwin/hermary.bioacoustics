
#' Title
#'
#' @param deployment_df
#' @param input_dir
#' @param temp_dir
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples

copy_compress_flacs <- function(deployment_df, input_dir, temp_dir, output_dir) {

  # List all .wav files in the input directory
  wav_files <- list.files(input_dir, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)

  # create pond/swift/date folders if necessary
  # name output files based on wav files
  swift <- stringr::str_extract(wav_files[1], 'S[0-9]+')

  pond <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::pull(pond_id)

  visit <-
    deployment_df |>
    dplyr::filter(swift_id == swift) |>
    dplyr::pull(visit_id)

  wav_files |>
    purrr::walk(\(x) create_temp_folders(wav_file = x, input_dir = input_dir, temp_dir = temp_dir))

  wav_files |>
    purrr::walk(\(x) create_flac_folders(wav_file = x, output_dir, pond_id = pond, visit_id = visit, swift_id = swift))

  wav_files |>
    furrr::future_walk(\(x) convert_to_flac(wav_file = x, input_dir = input_dir, temp_dir = temp_dir, output_dir = output_dir, pond_id = pond, visit_id = visit, swift_id = swift))

  message("FLAC compression completed!")

  beepr::beep()

}
