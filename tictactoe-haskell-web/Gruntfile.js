module.exports = function(grunt) {

    // Project configuration.
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        fay: {
            default_options: {
                options: {
                    // Task-specific options go here.
                },
                files: {
                    'dist/ttt.js':  ['src/**/*.hs', '../tictactoe-haskell-core/src/**/*.hs']
                }
            }
        }
    });

    // Load the plugin that provides the "fay" task.
    grunt.loadNpmTasks('grunt-fay');

    // Default task(s).
    grunt.registerTask('default', ['fay']);

};
