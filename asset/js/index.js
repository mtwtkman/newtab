const BOOKMARK_NOT_FOUND = -1
const STORAGE_KEY = "newtab-bookmarks"

function getJsonValue(key, defaultValue) {
  const value = localStorage.getItem(key) || null
  if (value === null) { return defaultValue }
  return JSON.parse(value)
}

function setJsonValue(key, value) {
  localStorage.setItem(key, JSON.stringify(value))
}

function fetchBookmarks() {
  return getJsonValue(STORAGE_KEY, [])
}

function findBookmarkIndex(bookmarks, title) {
  for (let i; i < bookmarks.length; i++) {
    if (bookmarks[i]["title"] == title) return i
  }
  return BOOKMARK_NOT_FOUND
}

function addBookmark(url, title) {
  const bookmarks = fetchBookmarks()
  const newBookmark = { title, url }
  const index = findBookmarkIndex(bookmarks, title)
  if (index === BOOKMARK_NOT_FOUND) {
    bookmarks.push(newBookmark)
  } else {
    bookmarks.splice
  }
  bookmarks.splice(index, 1, newBookmark)
  setJsonValue(STORAGE_KEY, bookmarks)
}

window.App = function(regionId) {
  const node = document.getElementById(regionId)
  const app = Elm.Main.init({
    node,
    flags: fetchBookmarks(),
  })
  app.ports.addBookmark.subscribe(message => {
    addBookmark(message[0], message[1])
    app.ports.messageReceiver.send(fetchBookmarks())
  })
}
