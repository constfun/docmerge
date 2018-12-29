const mockRequire = require('mock-require')
mockRequire('../backend', '../backend-orig')
require('../test/test')
