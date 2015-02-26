require! {
  'gulp-purescript': purescript
  'gulp': gulp
}

source = 

  * './bower_components/purescript-*/src/**/*.purs' 
  * './bower_components/purescript-*/src/**/*.purs.hs' 
  * './client/src/**/*.purs'


gulp.task \purescript ->
  gulp.src source
    .pipe purescript.psc do
      output: 'app.js'
      main: 'Main'
    .pipe gulp.dest 'public/scripts/'

