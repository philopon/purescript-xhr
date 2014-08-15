module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    port: 3000,

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],

    clean: {
      tests: ["tmp"],
      lib:   ["output"]
    },

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    docgen: {
        readme: {
            src: "src/**/*.purs",
            dest: "docs/Module.md"
        }
    },

    express: {
      test: {
        options: {
          script: 'tests/server.js',
          port: "<%=port%>"
        }
      }
    },

    watch: {
      dev: {
        files: ["src/**/*.purs", "tests/**/*"],
        tasks: ['psc:tests', 'express:test'],
        options: {spawn: false}
      }
    },

    mocha_phantomjs: {
      test: {
        options: {
          urls: ['http://localhost:<%=port%>']
        }
      }
    },

    psc: {
      tests: {
        options: {
          modules: ["Main"],
          main: true
        },
        src: ["tests/Test.purs", "<%=libFiles%>"],
        dest: "tmp/tests.js"
      }
    },

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  grunt.loadNpmTasks("grunt-express-server");
  grunt.loadNpmTasks("grunt-contrib-watch");
  grunt.loadNpmTasks('grunt-mocha-phantomjs');

  grunt.registerTask("make", ["pscMake", "dotPsci", "docgen"]);
  grunt.registerTask("dev", ["psc:tests", 'express:test', 'watch:dev']);
  grunt.registerTask("test", ["psc:tests", 'express:test', 'mocha_phantomjs:test']);
  grunt.registerTask("default", ["make"]);
};
