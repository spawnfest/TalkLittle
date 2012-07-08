websocket = null

MOUSE_GRID_ROWS = 4
MOUSE_GRID_COLS = 4

$senderMouse = null

# EventSource is used by receivers
setupEventSource = ->
  source = new EventSource '/eventsource/live'
  
  f = (event) ->
    if event.data == 'sender_you' and not websocket?
      startSending()
    else if event.data == 'sender_someone'
      stopSending()
    else if event.data.indexOf 'click:' == 0
      id = event.data.substr 6
      (jQuery "##{id}").trigger 'click' if id
    else if event.data.indexOf 'mouseenter:' == 0
      id = event.data.substr 11
      offset = (jQuery "##{id}").offset()
      drawMouse offset.top, offset.left
    else
      addStatus "server sent the following: '#{event.data}'"
  source.addEventListener 'message', f, false
  
  f = (event) ->
    addStatus 'eventsource connected.'
  source.addEventListener 'open', f, false
  
  f = (event) ->
    if event.eventPhase == EventSource.CLOSED
      addStatus 'eventsource was closed.'
  source.addEventListener 'error', f, false
  
startSending = ->
  console.log "startSending"
  setupWebsocket()
  addMouseListeners()
  
stopSending = ->
  console.log "stopSending"
  stopWebsocket()
  removeMouseListeners()
  
# WebSocket is used by sender
setupWebsocket = ->
  if "MozWebSocket" of window
    window.WebSocket = MozWebSocket
  if "WebSocket" of window
    # browser supports websockets
    websocket = new WebSocket "ws://localhost:8080/websocket"
    websocket.onopen = ->
      # websocket is connected
      addStatus "websocket connected!"
      # send hello data to server.
      websocket.send "hello server!"
      addStatus "sent message to server: 'hello server'!"
      
    websocket.onmessage = (event) ->
      if event.data == 'sender_you' and not websocket?
        startSending()
      else if event.data == 'sender_someone'
        stopSending()
      else
        addStatus "server sent the following: '#{event.data}'"
    
    websocket.onclose = ->
      # websocket was closed
      websocket = null
      addStatus "websocket was closed"

  else
    # browser does not support websockets
    addStatus "sorry, your browser does not support websockets."
  
stopWebsocket = ->
  websocket?.close()
  
addStatus = (text) ->
  date = new Date
  (document.getElementById 'status').innerHTML =
    (document.getElementById 'status').innerHTML + "#{date}: #{text}<br/>"

addMouseListeners = ->
  (jQuery ':button').click ->
    recordClick this
  
  # TODO use backbone.js in a future version
  mouseGrid = jQuery '<table>'
  for rid in [1..MOUSE_GRID_ROWS]
    row = jQuery '<tr>'
    for cid in [1..MOUSE_GRID_COLS]
      col = jQuery '<td>'
      gridId = MOUSE_GRID_COLS * (rid-1) + cid
      col.attr "id", "row-#{rid}-col-#{cid}"
      row.append col
    mouseGrid.append row
  mouseGrid.attr 'id', 'web-ui-broadcast-mouse-grid'
  mouseGrid.css 'width', '100%'
  mouseGrid.css 'height', '100%'
  mouseGrid.css 'position', 'absolute'
  mouseGrid.css 'top', '0px'
  mouseGrid.css 'left', '0px'
  (jQuery 'body').append mouseGrid
  
  (jQuery '#web-ui-broadcast-mouse-grid td').mouseenter ->
    recordMouseenter this
    
recordClick = (el) ->
  buttonId = (jQuery el).attr 'id'
  if buttonId?
    console.log "click:#{buttonId}"
    websocket.send "click:#{buttonId}"
    
recordMouseenter = (el) ->
  gridId = (jQuery el).attr 'id'
  if gridId?
    console.log "mouseenter:#{gridId}"
    websocket.send "mouseenter:#{gridId}"
    
drawMouse = (top, left) ->
  $senderMouse.offset top:top, left:left
  
initSenderMouse = ->
  $senderMouse = jQuery '#sender-mouse'

# main
if window.EventSource?
  initSenderMouse()
  setupEventSource()
  startSending()  # server will kill the websocket if we are not the sender, otherwise socket will stay open
else
  (document.getElementById 'status').innerHTML =
    "Sorry but your browser doesn't support the EventSource API";
