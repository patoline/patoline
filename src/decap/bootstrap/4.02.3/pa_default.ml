module ParserExt = Pa_parser.Ext(Pa_ocaml_prelude.Initial)
module Default = Pa_ocaml.Make(ParserExt)
module M = Pa_main.Start(Default)
