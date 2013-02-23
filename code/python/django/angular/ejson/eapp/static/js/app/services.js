(function(App) {
    'use strict';

    angular.module('ejson.service', [])
        .factory('Api', function($http) {
            var configure = {
                    serverDomain: null,
                    namespace: 'api/v1'
                },
                Api = function(data) {
                    angular.extend(this, data);
                },
                removeTrailingSlash = function(url) {
                    if (url.charAt(url.length - 1) === '/') {
                        return url.slice(0, -1);
                    }
                    return url;
                },
                getItemUrl = function(type, id) {
                    return removeTrailingSlash(['', configure.namespace, type, id, ''].join('/'))
                },
                camelize = function(data) { // <=
                    var c = function(dict) {
                            var d = new Api({});
                            $.each(dict, function(key, value) {
                                d[Em.String.camelize(key)] = value;
                            });
                            return d;
                        };

                    // {meta: ..., objects: ...}
                    if ('meta' in data && 'objects' in data) {
                        var d = c(data.meta);
                        d.objects = [];
                        $.each(data.objects, function(index, object) {
                            d.objects.push(c(object));
                        });
                        return d;
                    } 
                    return c(data);
                },
                decamelize = function(data) { // =>
                    var d = new Api({});
                    $.each(data, function(index, value) {
                        if (Object.prototype.toString.call(value) === '[object Object]') {
                            d[Em.String.decamelize(index+'_id')] = getItemUrl(Em.String.decamelize(index), value.id+'/');
                            return true; // continue
                        }
                        d[Em.String.decamelize(index)] = value;
                    });
                    return d;
                };

            // if your using django tastypie as the API locally be aware 
            // angular stips out the : (for example locahost:8000 wont work) 
            // try using sudo with port 80 (OSX's default port) which 
            // removes the need for using it
            delete $http.defaults.headers.common['X-Requested-With'];

            Api.find = function(type, id) {
                return $http.get(getItemUrl(type, id)).then(function(response) {
                    return camelize(response.data);
                });
            };

            Api.save = function(type, object) {
                if (!!object.id) {
                    return $http.put(getItemUrl(type, object.id+'/'), decamelize(object)).then(function(response) {
                        return camelize(response.data);
                    });
                }
                return $http.post(getItemUrl(type, null), decamelize(object)).then(function(response) {
                    return camelize(response.data);
                });
            }

            return Api;
        });

})(window.App);