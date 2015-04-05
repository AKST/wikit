require! {
  'gulp-purescript': purescript
  'gulp-concat': concat
  'gulp-shell': shell 
  'gulp-clean': clean
  'gulp-sass': sass
  'gulp': gulp
}


########################################################
#                     CONSTANTS                        # 
########################################################


app-out-name = \compiled-purescript.js


test-out-name = \compiled-test-purescript.js


lib-out-name = \jslibs.js


html-source = 'client/index.html'


scss-source = 'client/style/**/*.scss'


ps-client-source = './client/src/**/*.purs'


js-client-source = 'client/src.js/**/*.js'


temp-src-js = 
  * 'temp/jslibs.js'
  * 'temp/compiled-purescript.js'


temp-test-js =
  * 'temp/jslibs.js'
  * 'temp/compiled-test-purescript.js'


js-libs = 
  * './bower_components/es5-shim/es5-shim.min.js'
  * './bower_components/jquery/dist/jquery.js'
  * './bower_components/react/react.js'
  * './bower_components/rsvp/rsvp.js'
  * js-client-source


ps-source = 
  * './bower_components/purescript-*/src/**/*.purs' 
  * './bower_components/purescript-*/src/**/*.purs.hs' 
  * ps-client-source


ps-tests = ps-source.concat do 
  * './client/tests/**/*.purs'


client-src = [ps-client-source, js-client-source] 


########################################################
#                 JAVASCRIPT CONCAT                    # 
########################################################


gulp.task \js-src-build <[js-libs ps-src-build]> ->
  gulp.src temp-src-js
    .pipe concat \app.js
    .pipe gulp.dest 'public/scripts'


gulp.task \js-test-build <[js-libs ps-test-build]> ->
  gulp.src temp-test-js
    .pipe concat \tests.js
    .pipe gulp.dest 'public/scripts'


gulp.task \js-libs ->
  gulp.src js-libs
    .pipe concat lib-out-name
    .pipe gulp.dest \temp


########################################################
#                  PURESCIPRT BUILD                    # 
########################################################


gulp.task \ps-src-build ->
  gulp.src ps-source
    .pipe purescript.psc do
      output: app-out-name
      main: \Main
    .on \error (err) !->
      console.error err.message ? err
    .pipe gulp.dest \temp


gulp.task \ps-test-build ->
  gulp.src ps-tests
    .pipe purescript.psc do
      output: test-out-name
      main: \AllTests
    .pipe gulp.dest \temp


########################################################
#                     STYLE BUILD                      # 
########################################################


gulp.task \scss ->
  gulp.src './client/style/app.scss'
    .pipe sass!
    .pipe gulp.dest './public/style'


########################################################
#                     FILE MOVING                      # 
########################################################


gulp.task \html ->
  gulp.src html-source
    .pipe gulp.dest 'public/.'


########################################################
#                    HOUSE CLEANING                    # 
########################################################


gulp.task \clean ->
  gulp.src <[temp public]> { read: false }
    .pipe clean!


########################################################
#                        TESTS                         # 
########################################################


gulp.task \test <[js-test-build]> ->
  #
  # not sure if this file is necessary but
  # the docs for gulp-shell say include it
  # so whatever
  #
  gulp.src './public/scripts/tests.js'
    .pipe shell do 
      * 'phantomjs <%= file.path %>'


########################################################
#                        WATCH                         # 
########################################################


gulp.task \watch <[js-src-build html scss]> !->
  gulp.watch client-src, [\js-src-build]
  gulp.watch html-source, [\html]
  gulp.watch scss-source, [\scss]


