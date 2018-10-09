const { ROOT_ID } = require('../src/common')
const { CHANGE } = require('./constants')
const { Text } = require('./text')

function parseListIndex(key) {
  if (typeof key === 'string' && /^[0-9]+$/.test(key)) key = parseInt(key)
  if (typeof key !== 'number') {
    throw new TypeError('A list index must be a number, but you passed ' + JSON.stringify(key))
  }
  if (key < 0 || isNaN(key) || key === Infinity || key === -Infinity) {
    throw new RangeError('A list index must be positive, but you passed ' + key)
  }
  return key
}

function listMethods(context, listId) {
  const methods = {
    deleteAt(index, numDelete) {
      context.splice(listId, parseListIndex(index), numDelete || 1, [])
      return this
    },

    fill(value, start, end) {
      let list = context.getObject(listId)
      for (let index = parseListIndex(start || 0); index < parseListIndex(end || list.length); index++) {
        context.setListIndex(listId, index, value)
      }
      return this
    },

    insertAt(index, ...values) {
      context.splice(listId, parseListIndex(index), 0, values)
      return this
    },

    pop() {
      let list = context.getObject(listId)
      if (list.length == 0) return
      const last = context.getObjectField(listId, list.length - 1)
      context.splice(listId, list.length - 1, 1, [])
      return last
    },

    push(...values) {
      let list = context.getObject(listId)
      context.splice(listId, list.length, 0, values)
      // need to getObject() again because the list object above may be immutable
      return context.getObject(listId).length
    },

    shift() {
      let list = context.getObject(listId)
      if (list.length == 0) return
      const first = context.getObjectField(listId, 0)
      context.splice(listId, 0, 1, [])
      return first
    },

    splice(start, deleteCount, ...values) {
      let list = context.getObject(listId)
      start = parseListIndex(start)
      if (deleteCount === undefined) {
        deleteCount = list.length - start
      }
      const deleted = []
      for (let n = 0; n < deleteCount; n++) {
        deleted.push(context.getObjectField(listId, start + n))
      }
      context.splice(listId, start, deleteCount, values)
      return deleted
    },

    unshift(...values) {
      context.splice(listId, 0, 0, values)
      return context.getObject(listId).length
    }
  }

  for (let iterator of ['entries', 'keys', 'values']) {
    let list = context.getObject(listId)
    methods[iterator] = () => list[iterator]()
  }

  // Read-only methods that can delegate to the JavaScript built-in implementations
  for (let method of ['concat', 'every', 'filter', 'find', 'findIndex', 'forEach', 'includes',
                      'indexOf', 'join', 'lastIndexOf', 'map', 'reduce', 'reduceRight',
                      'slice', 'some', 'toLocaleString', 'toString']) {
    methods[method] = (...args) => {
      const list = context.getObject(listId)
      return list[method].call(list, ...args)
    }
  }

  return methods
}

const MapHandler = {
  get (target, key) {
    const { context, objectId } = target
    if (key === '_inspect') return JSON.parse(JSON.stringify(mapProxy(context, objectId)))
    if (key === '_type') return 'map'
    if (key === '_objectId') return objectId
    if (key === CHANGE) return context
    if (key === '_get') return context._get
    return context.getObjectField(objectId, key)
  },

  set (target, key, value) {
    const { context, objectId } = target
    context.setMapKey(objectId, key, value)
    return true
  },

  deleteProperty (target, key) {
    const { context, objectId } = target
    context.deleteMapKey(objectId, key)
    return true
  },

  has (target, key) {
    const { context, objectId } = target
    return ['_type', '_objectId', CHANGE, '_get'].includes(key) || (key in context.getObject(objectId))
  },

  getOwnPropertyDescriptor (target, key) {
    const { context, objectId } = target
    const object = context.getObject(objectId)
    if (key in object) {
      return {configurable: true, enumerable: true}
    }
  },

  ownKeys (target) {
    const { context, objectId } = target
    return Object.keys(context.getObject(objectId))
  }
}

const ListHandler = {
  get (target, key) {
    const [context, objectId] = target
    if (key === Symbol.iterator) return context.getObject(objectId)[Symbol.iterator]
    if (key === '_inspect') return JSON.parse(JSON.stringify(listProxy(context, objectId)))
    if (key === '_type') return 'list'
    if (key === '_objectId') return objectId
    if (key === CHANGE) return context
    if (key === 'length') return context.getObject(objectId).length
    if (typeof key === 'string' && /^[0-9]+$/.test(key)) {
      return context.getObjectField(objectId, parseListIndex(key))
    }
    return listMethods(context, objectId)[key]
  },

  set (target, key, value) {
    const [context, objectId] = target
    context.setListIndex(objectId, parseListIndex(key), value)
    return true
  },

  deleteProperty (target, key) {
    const [context, objectId] = target
    context.splice(objectId, parseListIndex(key), 1, [])
    return true
  },

  has (target, key) {
    const [context, objectId] = target
    if (typeof key === 'string' && /^[0-9]+$/.test(key)) {
      return parseListIndex(key) < context.getObject(objectId).length
    }
    return ['length', '_type', '_objectId', CHANGE].includes(key)
  },

  getOwnPropertyDescriptor (target, key) {
    if (key === 'length') return {}
    if (key === '_objectId') return {configurable: true, enumerable: false}

    const [context, objectId] = target
    const object = context.getObject(objectId)

    if (typeof key === 'string' && /^[0-9]+$/.test(key)) {
      const index = parseListIndex(key)
      if (index < object.length) return {configurable: true, enumerable: true}
    }
  },

  ownKeys (target) {
    const [context, objectId] = target
    const object = context.getObject(objectId)
    let keys = ['length', '_objectId']
    keys.push(...Object.keys(object))
    return keys
  }
}

function mapProxy(context, objectId) {
  return new Proxy({context, objectId}, MapHandler)
}

function listProxy(context, objectId) {
  return new Proxy([context, objectId], ListHandler)
}

/**
 * Instantiates a proxy object for the given `objectId`.
 * This function is added as a method to the context object by rootObjectProxy().
 * When it is called, `this` is the context object.
 */
function instantiateProxy(objectId) {
  const object = this.getObject(objectId)
  if (Array.isArray(object) || (object instanceof Text)) {
    return listProxy(this, objectId)
  } else {
    return mapProxy(this, objectId)
  }
}

function rootObjectProxy(context) {
  context.instantiateObject = instantiateProxy
  context._get = (objId) => instantiateProxy.call(context, objId)
  return mapProxy(context, ROOT_ID)
}

module.exports = { rootObjectProxy }
