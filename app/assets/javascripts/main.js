function HashMap(obj) {
    this.length = 0;
    this.items = {};
    for (var p in obj) {
        if (obj.hasOwnProperty(p)) {
            this.items[p] = obj[p];
            this.length++;
        }
    }
    this.put = function(key, value) {
        var previous = undefined;
        if (this.containsKey(key)) {
            previous = this.items[key];
        } else {
            this.length++;
        }
        this.items[key] = value;
        return previous;
    }
    this.putIfAbsent = function(key, value, func) {
        var previous = undefined;
        if (this.containsKey(key)) {
            previous = this.items[key];
        } else {
            this.length++;
            func()
        }
        this.items[key] = value;
        return previous;
    }
    this.get = function(key) {
        return this.containsKey(key) ? this.items[key] : undefined;
    }
    this.containsKey = function(key) {
        return this.items.hasOwnProperty(key);
    }
    this.remove = function(key) {
        if (this.containsKey(key)) {
            previous = this.items[key];
            this.length--;
            delete this.items[key];
            return previous;
        } else {
            return undefined;
        }
    }
    this.keys = function() {
        var keys = [];
        for (var k in this.items) {
            if (this.containsKey(k)) {
                keys.push(k);
            }
        }
        return keys;
    }
    this.values = function() {
        var values = [];
        for (var k in this.items) {
            if (this.containsKey(k)) {
                values.push(this.items[k]);
            }
        }
        return values;
    }
    this.each = function(fn) {
        for (var k in this.items) {
            if (this.containsKey(k)) {
                fn(k, this.items[k]);
            }
        }
    }
    this.clear = function() {
        this.items = {}
        this.length = 0;
    }
}

var GAME = (typeof module !== "undefined" && module.exports) || {};

(function (exports) {
    exports.name = "main.js";
    exports.version = "1.0.0";
    exports.log = log;
    exports.debug = debug;
    exports.trace = trace;
    exports.startsWith = startsWith;
    exports.param = param;

    function log(message) {
        console.log("[INFO] %s", message)
    }

    function debug(message) {
        console.log("[DEBUG] %s", message)
    }

    function trace(message) {
        console.log("[TRACE] %s", message)
    }

    function startsWith(value, starts) {
        return (value.match("^"+starts)==starts)
    }

    function param(name){
        var strReturn = "";
        var strHref = window.location.href;
        if ( strHref.indexOf("?") > -1 ){
            var strQueryString = strHref.substr(strHref.indexOf("?")).toLowerCase();
            var aQueryString = strQueryString.split("&");
            for ( var iParam = 0; iParam < aQueryString.length; iParam++ ){
                if ( 
                aQueryString[iParam].indexOf(name.toLowerCase() + "=") > -1 ){
                    var aParam = aQueryString[iParam].split("=");
                    strReturn = aParam[1];
                    break;
                }
            }
        }
        return unescape(strReturn);
    }

    String.prototype.startsWith = function(str) {
        return startsWith(this, str)
    }

})(GAME);