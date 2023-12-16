open Ewe

module Counter : App.Component = struct
  module Model = struct
    type t = int
  end

  module Action = struct
    type t =
      | Increment
      | Decrement

    let apply action m =
      match action with
      | Increment -> m + 1
      | Decrement -> m - 1
    ;;
  end

  let view model _action =
    let open Incremental.Let_syntax in
    let open Vdom in
    let%map model = model in
    node
      ~id:"CounterW"
      ~grid:(coldef [ px 200 ])
      ~attrs:[]
      [ text
          ~id:"Counter"
          ~attrs:[]
          ~grid:(colidx2 0 0)
          (Format.sprintf "Counter -> %d" model)
          []
      ]
  ;;

  let initial_model () = 1
  let temp_subchars = [ 'a'; 'd' ]

  let subscriptions (key : Events.Key.t) =
    let open Action in
    match key with
    | A -> Sub.Cmd Increment
    | D -> Sub.Cmd Decrement
    | _ -> Sub.Nil
  ;;
end

module Neofetch = struct
  module Model = struct
    type t = unit
  end

  module Action = struct
    type t = unit

    let apply _action m = m
  end

  let beige (left, right) = Format.sprintf "\x1b[38;5;223m%s\x1b[38;5;188m%s" left right

  let view model _action =
    let open Incremental.Let_syntax in
    let open Vdom in
    let open Core in
    let%map _model = model in
    node
      ~id:"root"
      ~grid:(colidx 0)
      ~attrs:[]
      [ image
          ~id:"root"
          ~attrs:[]
          ~grid:(colidx2 0 0)
          ~size:(254, 254)
          "~/Desktop/neofetch.png"
          []
      ; text
          ~id:"neofetch"
          ~attrs:[]
          ~grid:(colidx2 0 1)
          (List.fold_left ~init:"" ~f:(fun acc x -> acc ^ x)
           @@ List.map
                ~f:beige
                [ "black@bonez\n", ""
                ; "----------\n", ""
                ; "OS: ", "Arch Linux x86_64\n"
                ; "Host: ", "80MK Lenovo YOGA 900-13ISK\n"
                ; "Kernel: ", "4.14.10-1-Arch\n"
                ; "Uptime: ", "4 hours, 32 mins\n"
                ; "Packages: ", "713\n"
                ; "Shell: ", "bash 4.4.12\n"
                ; "WM: ", "Openbox\n"
                ; "WM Theme: ", "Thicc\n"
                ; "Theme: ", "Lumiere [GTK2/3]\n"
                ; "Icons: ", "Paper [GTK2/3]\n"
                ; "Terminal: ", "xfce4-terminal\n"
                ; "Terminal Font: ", "Roboto Mono 12\n"
                ; "CPU: ", "Intel i7-6500U (4) @ 3.100GHz\n"
                ; "GPU: ", "Intel HD Graphics 520\n"
                ; "Memory: ", "1933MiB / 7890 MiB"
                ])
          []
      ]
  ;;

  let initial_model () = ()

  let mapping = function
    | _ -> None
  ;;
end
