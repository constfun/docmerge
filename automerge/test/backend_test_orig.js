const mockRequire = require('mock-require')
mockRequire('../backend', require('../backend-orig'))
require('./backend_test')
