with import <nixpkgs> {};
mkShell {
  packages = [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-language-server
    elmPackages.elm-test
    nodePackages.uglify-js
    nodePackages.vscode-html-languageserver-bin
    nodePackages.typescript-language-server
  ];
  shellHook = ''
    alias build:elm="elm make src/Main.elm --output=asset/js/app.js"
    alias build:js="mkdir -p asset/js && cp index.js asset/js/index.js"
    alias build="build:elm && build:js"
    alias dev:elm="build:elm --debug"
    alias dev="dev:elm && build:js"
    alias clean="rm -f asset"
    alias pub:js="uglifyjs index.js | uglifyjs --mangle --output asset/js/index.js"
    alias pub:elm="build:elm --optimize && uglifyjs asset/js/app.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output asset/js/app.js"
    alias pub="clean && pub:elm && pub:js"
    alias t="elm-test"
    alias fmt="elm-format --yes src/ tests/"
  '';
}
