require! {
  'gulp-purescript': purescript
  'gulp': gulp
}

gulp.task \purescript ->
  gulp.src 'client/src/**/*.purs.hs'
    .pipe purescript.psc!
    .pipe gulp.dest 'dist/'

