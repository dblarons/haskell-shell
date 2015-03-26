# A sample Guardfile
# More info at https://github.com/guard/guard#readme

repl_options =
  [ "--ghc-options=-ignore-dot-ghci" \
  ]

guard :haskell, repl_options: repl_options do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
end
