require! {
  'gulp-mocha-phantomjs': mocha-phantom-js 
  'gulp-livereload': livereload 
  'gulp-purescript': purescript
  'gulp-mustache': mustache
  'gulp-flatten': flatten
  'gulp-concat': concat
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


html-source = 'client/documents/*.html'


scss-source = 'client/style/**/*.scss'


ps-client-source = './client/src/**/*.purs'


ps-client-tests = './client/tests/**/*.purs'


js-client-source = 'client/js-src/**/*.js'


temp-src-js = 
  * 'temp/jslibs.js'
  * 'temp/compiled-purescript.js'


temp-test-js = 
  * 'temp/jslibs.js'
  * './bower_components/mocha/mocha.js'
  * './bower_components/chai/chai.js'
  * 'temp/compiled-test-purescript.js'


js-libs = 
  * './bower_components/es5-shim/es5-shim.min.js'
  * './bower_components/jquery/dist/jquery.js'
  * './bower_components/react/react.js'
  * './bower_components/rsvp/rsvp.js'
  * js-client-source


css-libs =
  * './bower_components/mocha/mocha.css'
  * './bower_components/skeleton-css/css/skeleton.css'


ps-source = 
  * './bower_components/purescript-*/src/**/*.purs' 
  * './bower_components/purescript-*/src/**/*.purs.hs' 
  * ps-client-source


ps-tests = ps-source.concat do 
  * ps-client-tests 


client-src = [ps-client-source, js-client-source, ps-client-tests] 


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
      browserNamespace: 'Wikit'
      output: app-out-name
      main: \Main
    .on \error (err) !->
      console.error err.message ? err
    .pipe gulp.dest \temp
    .pipe livereload()


gulp.task \ps-test-build ->
  gulp.src ps-tests
    .pipe purescript.psc do
      output: test-out-name
      main: \AllTests
    .on \error (err) !->
      console.error err.message ? err
    .pipe gulp.dest \temp


########################################################
#                     STYLE BUILD                      # 
########################################################


gulp.task \scss ->
  gulp.src './client/style/app.scss'
    .pipe sass!
    .pipe gulp.dest './public/style'
    .pipe livereload()


########################################################
#                     FILE MOVING                      # 
########################################################


gulp.task \html ->
  gulp.src html-source
    .pipe mustache do
      is-production: process.env.ENV == 'PROD'
    .pipe gulp.dest './public/.'
    .pipe livereload()

gulp.task \3rd-party-css ->
  gulp.src css-libs 
    .pipe gulp.dest './public/style'

gulp.task \icons ->
  gulp.src './bower_components/open-iconic/svg/*.svg'
    .pipe gulp.dest './public/svg'

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
  gulp.src './public/tests.html'
    .pipe mocha-phantom-js do
      reporter: 'spec'
      mocha: 
        slow: 75


########################################################
#                        WATCH                         # 
########################################################


gulp.task \watch <[js-src-build html scss 3rd-party-css icons]> !->
  livereload.listen { port: 12345, base-path: 'public' }
  gulp.watch client-src, <[js-src-build test]>
  gulp.watch html-source, [\html]
  gulp.watch scss-source, [\scss]


