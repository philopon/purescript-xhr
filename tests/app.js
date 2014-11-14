var express = require('express');
var app = express();
var bodyParser = require('body-parser')

app.use(bodyParser.urlencoded({extended: false}));
app.use(bodyParser.json());
app.use(require('connect-multiparty')());

app.use(express.static(__dirname + '/static'));
app.use('/mocha', express.static(__dirname + '/../node_modules/mocha'));
app.use('/chai', express.static(__dirname + '/../node_modules/chai'));
app.use('/js', express.static(__dirname + '/../tmp'));

app.get('/api/no_param', function(req, res){
  res.set('Content-Type', 'text/plain');
  res.send('no_param');
});

app.get('/api/param', function(req, res) {
  res.send('q param: ' + req.query.q);
});

app.get("/api/headers", function(req, res) {
  res.send('X-Test-Header: ' + req.headers['x-test-header']);
});

app.post('/api/body', function(req, res) {
  res.send('q body: ' + req.body.q);
});

module.exports = app;
