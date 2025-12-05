let read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_lines

let read_string file_name =
  In_channel.with_open_text file_name In_channel.input_all