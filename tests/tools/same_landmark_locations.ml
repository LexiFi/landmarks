open Ppxlib

let has_landmark_attribute attributes =
  List.exists
    (fun { attr_name = { txt; _ }; _ } -> String.equal txt "landmark")
    attributes

let erase_landmark_locations =
  object
    inherit Ast_traverse.map as super

    method! value_binding value_binding =
      let value_binding = super # value_binding value_binding in
      if has_landmark_attribute value_binding.pvb_attributes then
        { value_binding with pvb_loc = Location.none }
      else
        value_binding
  end

let () =
  Driver.register_transformation "same-landmark-locations"
    ~preprocess_impl:(erase_landmark_locations # structure)

let registered = ()
