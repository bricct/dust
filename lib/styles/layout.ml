open Notty

let grid xxs = xxs |> List.map I.hcat |> I.vcat

let center w h img = 
  let open I in
  let align = `Middle in
  hsnap ~align w img |> 
  vsnap ~align h 

let block ?(attr = A.empty) = I.char attr ' '

let pad ?(l=0) ?(t=0) ?(r=0) ?(b=0) ?(attr = A.empty) img =
  let create_pad w h attr = 
    if (w = 0 || h = 0) then I.empty else I.char attr ' ' w h
  in
  let img_height = I.height img in
  let inner = I.hcat [ create_pad l img_height attr; img; create_pad r img_height attr ] in
  let img_width = I.width inner in
  I.vcat [ create_pad img_width t attr; inner; create_pad img_width b attr]

let flex_v ?gap ?(align = `Left) ?(attr = A.empty) images  =
  let compose_with_gaps padded_imgs width gap = 
    let gap_box = I.char attr ' ' width gap in
    let rec interleave_with_gaps imgs out = 
      match imgs with
      | [] -> []
      | i :: [] -> List.rev (i :: out)
      | i :: is -> interleave_with_gaps is (I.( <-> ) i gap_box :: out)
    in
    I.vcat @@ interleave_with_gaps padded_imgs []
  in
  let compose imgs width =
    match gap with
    | None -> I.vcat imgs
    | Some g -> compose_with_gaps imgs width g
  in
  let pad width img =
    let height = I.height img in
    let img_width = I.width img in
    let x_diff = (width - img_width) in
    if x_diff = 0 then
      img
    else
      let full_pad = I.char attr ' '  x_diff height in
      let left_pad = I.char attr ' ' (x_diff / 2) height in
      let right_pad = I.char attr ' ' ((x_diff / 2) + (x_diff mod 2)) height in
      match align with
      | `Left -> I.( <|> ) img full_pad
      | `Right -> I.( <|> ) full_pad img 
      | `Middle -> I.hcat [left_pad; img; right_pad]
  in
  let max_width = List.fold_left (fun w i -> max w (I.width i)) 0 images in
  let padded_images = List.map (pad max_width) images in
  compose padded_images max_width

let justify ?(attr = A.empty) width images =
  let total_width, len = List.fold_left (fun (w, l) i -> w + I.width i, l + 1) (0, 0) images in
  let remaining = width - total_width in
  if remaining < 0 then 
    I.string A.empty "Images are too big for space" 
  else
    match images with
    | [] -> I.empty
    | img :: images ->
      let base = remaining / len in 
      let ex = remaining mod len in
      let folder (img, extra) i = 
        let l, extra = 
          if extra > 0 then
            base + 1, extra - 1
          else
            base, extra
        in
        let rhi = pad ~l ~attr i in
        (I.(img <|> rhi), extra)
      in
      fst @@ List.fold_left folder (img, ex) images

let flex_h ?gap ?(align = `Top) ?(attr = A.empty) images  =
  let compose_with_gaps padded_imgs height gap = 
    let gap_box = I.char attr ' ' gap height in
    let rec interleave_with_gaps imgs out = 
      match imgs with
      | [] -> []
      | i :: [] -> List.rev (i :: out)
      | i :: is -> interleave_with_gaps is (I.( <|> ) i gap_box :: out)
    in
    I.hcat @@ interleave_with_gaps padded_imgs []
  in
  let compose imgs height =
    match gap with
    | None -> I.hcat imgs
    | Some g -> compose_with_gaps imgs height g
  in
  let pad height img =
    let width = I.width img in
    let img_height = I.height img in
    let y_diff = (height - img_height) in
    if y_diff = 0 then
      img
    else
      let full_pad = I.char attr ' '  width y_diff in
      let top_pad = I.char attr ' '  width (y_diff / 2) in
      let bottom_pad = I.char attr ' ' width ((y_diff / 2) + (y_diff mod 2)) in
      match align with
      | `Top -> I.( <-> ) img full_pad
      | `Bottom -> I.( <-> ) full_pad img 
      | `Middle -> I.vcat [top_pad; img; bottom_pad]
  in
  let max_height = List.fold_left (fun h i -> max h (I.height i)) 0 images in
  let padded_images = List.map (pad max_height) images in
  compose padded_images max_height


let box ?width ?height ?(v_align = `Middle) ?(h_align = `Middle) ?(attr = A.empty) img = 
  let width_diff = match width with
  | None -> 0
  | Some w -> max (w - I.width img) 0
  in
  let height_diff = match height with
  | None -> 0
  | Some h -> max (h - I.height img) 0
  in
  if width_diff = 0 && height_diff = 0 then img
  else
    let split number = 
      (number / 2), ((number / 2) + (number mod 2))
    in
    let t, b = match v_align with
    | `Top -> 0, height_diff
    | `Middle -> split height_diff
    | `Bottom -> height_diff, 0
    in
    let l, r = match h_align with
    | `Left -> 0, width_diff
    | `Middle -> split width_diff
    | `Right -> width_diff, 0
    in
  pad ~l ~r ~t ~b ~attr img


let flex ?gap ?(align = `Top) ?(attr = A.empty) images  =
  let compose_pair = I.( <|> ) in
  let compose_list = I.hcat in

  let compose_with_gaps padded_imgs independent_dimension gap = 
    let gap_box_width, gap_box_height =
      gap, independent_dimension
    in

    let gap_box = I.char attr ' ' gap_box_width gap_box_height in
    let rec interleave_with_gaps imgs out = 
      match imgs with
      | [] -> []
      | i :: [] -> List.rev (i :: out)
      | i :: is -> interleave_with_gaps is (compose_pair i gap_box :: out)
    in
    compose_list @@ interleave_with_gaps padded_imgs []
  in
  let compose imgs independent_dimension =
    match gap with
    | None -> compose_list imgs
    | Some g -> compose_with_gaps imgs independent_dimension g
  in
  let pad max_dimension img =
    let width = I.width img in
    let height = I.height img in
    let dim_diff = (max_dimension - height) in
    if dim_diff = 0 then
      img
    else
      let w, h = width, dim_diff in
      let half_w, half_h = width,  (dim_diff / 2) in
      let second_half_w, second_half_h = width, ((dim_diff / 2) + (dim_diff mod 2)) in
      let full_pad () = I.char attr ' '  w h in
      let top_pad () = I.char attr ' '  half_w half_h in
      let bottom_pad () = I.char attr ' ' second_half_w second_half_h in

      match align with
      | `Top -> I.( <-> ) img (full_pad ())
      | `Bottom -> I.( <-> ) (full_pad ()) @@ img 
      | `Middle -> I.vcat [(top_pad ()); img; (bottom_pad ())]
  in
  let max_dimension = List.fold_left (fun h i -> max h (I.height i)) 0 images in
  let padded_images = List.map (pad max_dimension) images in
  compose padded_images max_dimension
