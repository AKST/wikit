require! {
  'gulp-purescript': purescript
  'gulp': gulp
}


html-source = 'client/index.html'


ps-source = 
  * './bower_components/purescript-*/src/**/*.purs' 
  * './bower_components/purescript-*/src/**/*.purs.hs' 
  * client-src = './client/src/**/*.purs'


gulp.task \purescript ->
  gulp.src ps-source
    .pipe purescript.psc do
      output: 'app.js'
      main: 'Main'
    .pipe gulp.dest 'public/scripts/'


gulp.task \html ->
  gulp.src html-source
    .pipe gulp.dest 'public/.'


gulp.task \watch <[purescript html]> !->
  gulp.watch client-src, [\purescript]
  gulp.watch html-source, [\html]


