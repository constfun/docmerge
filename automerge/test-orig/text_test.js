const mockRequire = require('mock-require')
mockRequire('../backend', '../backend-orig')
require('../test/text_test')
