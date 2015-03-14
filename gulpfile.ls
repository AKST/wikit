require! {
  'gulp-purescript': purescript
  'gulp-concat': concat
  'gulp-clean': clean
  'gulp-sass': sass
  'gulp': gulp
}


html-source = 'client/index.html'


scss-source = 'client/style/**/*.scss'


temp-js = 
  * 'temp/jslibs.js'
  * 'temp/compiled-purescript.js'


js-libs = 
  * './bower_components/jquery/dist/jquery.js'
  * './bower_components/react/react.js'


ps-source = 
  * './bower_components/purescript-*/src/**/*.purs' 
  * './bower_components/purescript-*/src/**/*.purs.hs' 
  * client-src = './client/src/**/*.purs'


gulp.task \purescript !->
  gulp.src ps-source
    .pipe purescript.psc do
      output: \compiled-purescript.js
      main: \Main
    .on \error (err) !->
      console.error err.message ? err
    .pipe gulp.dest \temp


gulp.task \js-libs !->
  gulp.src js-libs
    .pipe concat \jslibs.js
    .pipe gulp.dest \temp


gulp.task \js <[purescript js-libs]> !->
  gulp.src temp-js
    .pipe concat \app.js
    .pipe gulp.dest 'public/scripts'


gulp.task \scss !->
  gulp.src './client/style/app.scss'
    .pipe sass!
    .pipe gulp.dest './public/style'


gulp.task \html !->
  gulp.src html-source
    .pipe gulp.dest 'public/.'


gulp.task \clean !->
  gulp.src <[temp public]> { read: false }
    .pipe clean!


# gulp.task \watch <[js html scss]> !->
gulp.task \watch <[js html]> !->
  gulp.watch client-src, [\js]
  gulp.watch html-source, [\html]
  gulp.watch scss-source, [\scss]


