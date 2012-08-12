define([

], function() {
    function mk_xhr(url, synchronous) {
        var xhr;

        xhr = new XMLHttpRequest();
        xhr.timeout = 0;

        xhr.open("POST", url, !synchronous);

        return xhr;
    }

    function send_post(xhr, params) {
        var post_str;

        post_str = "";
        for (key in params) {
            if (params.hasOwnProperty(key)) {
                post_str += key + "=" + params[key] + "&";
            }
        }
        post_str = post_str.substr(0, post_str.length - 1);

        xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        xhr.send(post_str);
    }

    function Comet(url, params, handle_evt, handle_close) {
        var self;

        self = this;

        self.open = function() {
            self.last_ix = 0;
            self.xhr = mk_xhr(url);
            self.open = true;
            self.aborted = false;

            self.xhr.onreadystatechange = function() {
                if (self.xhr.readyState === 3) {
                    self.update();
                } else if (self.xhr.readyState === 4) {
                    self.open = false;
                    if (!self.aborted) {
                        if (handle_close) {
                            handle_close();
                        }
                        self.open();
                    }
                }
            };

            send_post(self.xhr, params);
        };

        self.update = function() {
            var ix, resp;

            ix = self.last_ix;
            resp = self.xhr.responseText;

            if (ix != resp.length) {
                self.last_ix = resp.length;
                handle_evt(JSON.parse(resp.substr(ix)));
            }
        };

        self.close = function() {
            if (self.open) {
                self.aborted = true;
                self.xhr.abort();
            }
        };

        self.open();
    };

    function json_post(url, params, success, synchronous) {
        var xhr;

        xhr = mk_xhr(url, synchronous);
        if (success) {
            xhr.onreadystatechange = function() {
                if (xhr.readyState === 4) {
                    success(JSON.parse(xhr.responseText));
                }
            };
        }

        send_post(xhr, params);
    }

    return {
        Comet: Comet,
        json_post: json_post
    };
});
