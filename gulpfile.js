var gulp       = require('gulp');
var purescript = require('gulp-purescript');
var foreach    = require('gulp-foreach');
var karma      = require('gulp-karma');

var path       = require('path');

var bowerPurs = 'bower_components/purescript-*/src/**/*.purs';
var sources = [bowerPurs, 'src/**/*.purs'];

gulp.task('pscMake', function(){
  return gulp
    .src(sources)
    .pipe(purescript.pscMake());
});

gulp.task('dotPsci', function(){
  return gulp
    .src(sources)
    .pipe(purescript.dotPsci());
});

gulp.task('pscDocs', function(){
  return gulp
    .src('src/**/*.purs')
    .pipe(foreach(function(stream, file){
      var p = path.resolve(
        'docs',
        path.dirname(file.relative),
        path.basename(file.relative, ".purs") + ".md")
      return stream
        .pipe(purescript.pscDocs())
        .pipe(gulp.dest(p));
    }));
});

gulp.task('ci', function(){
  return gulp
    .src(sources.concat('tests/Test.purs'))
    .pipe(purescript.psc({main: 'Test.Main', output: 'test.js'}))
    .pipe(gulp.dest('tmp/'))
    .pipe(karma({configFile: "karma.conf-ci.js", action: "run"}));
});

gulp.task('test', function(){
  return gulp
    .src(sources.concat('tests/Test.purs'))
    .pipe(purescript.psc({main: 'Test.Main', output: 'test.js'}))
    .pipe(gulp.dest('tmp/'))
    .pipe(karma({configFile: "karma.conf.js", action: "run"}));
});

gulp.task('default', ['pscMake', 'dotPsci', 'pscDocs', 'test']);
