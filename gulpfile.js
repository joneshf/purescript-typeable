'use strict'

var gulp       = require('gulp')
  , purescript = require('gulp-purescript')
  ;

var paths = {
    src: 'src/**/*.purs',
    foreigns: 'src/**/*.js',
    bowerSrc: 'bower_components/purescript-*/src/**/*.purs',
    bowerForeigns: 'bower_components/purescript-*/src/**/*.js',
    dest: '',
    docsDest: 'README.md'
};

var options = {
  verboseErrors: false
};

var compile = function(compiler) {
    var psc = compiler(options);
    psc.on('error', function(e) {
        console.error(e.message);
        psc.end();
    });
    return gulp.src([paths.src].concat(paths.bowerSrc))
        .pipe(psc)
        .pipe(gulp.dest(paths.dest));
};

gulp.task('make', function() {
    return purescript.psc({
        src: [paths.src, paths.bowerSrc],
        ffi: [paths.foreigns, paths.bowerForeigns]
    });
});

gulp.task('browser', function() {
    return compile(purescript.psc);
});

gulp.task("docs", ["make"], function () {
    return purescript.pscDocs({
        src: [paths.src, paths.bowerSrc],
        format: "markdown",
        docgen: {
            "Data.Typeable": paths.docsDest
        }
    });
});

gulp.task('watch-browser', function() {
    gulp.watch(paths.src, ['browser', 'docs']);
});

gulp.task('watch-make', function() {
    gulp.watch(paths.src, ['make', 'docs']);
});

gulp.task('default', ['make', 'docs']);
