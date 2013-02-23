(function(win) {
	'use strict';

	var App = angular.module('ejson', ['ejson.service', 'ejson.directive', 'ejson.filter']).
		config(['$routeProvider', function($routeProvider) {
	    	$routeProvider
	    		.when('/', {
	    			templateUrl: '/static/js/app/partials/notebook_list.html', 
	    			controller: 'NotebookCtrl'
	    		})
	    		.when('/notebook/:id', {
	    			templateUrl: '/static/js/app/partials/notebook_show.html', 
	    			controller: 'NotebookShowCtrl'
	    		})
	    		.otherwise({
	    			redirectTo: '/'
	    		});
		}]);

	win.App = App;
})(window);