const mockRequire = require('mock-require')
mockRequire('../backend', require('../backend-js'))

require('./backend_test')
