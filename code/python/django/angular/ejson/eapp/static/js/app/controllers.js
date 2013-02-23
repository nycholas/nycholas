(function(App) {
	'use strict';

	App.controller('ApplicationCtrl', function($scope) {
		$scope.appName = 'Angular Example';
	});

	App.controller('NotebookCtrl', function($scope, $http) {
		$http.get('/notebook/').then(function(response) {
			$scope.notebooks = response.data;
		});
	});

	App.controller('NotebookShowCtrl', function($scope, $http, $routeParams) {
		$scope.notebook = $http.get('/notebook/' + $routeParams.id + '/');
	});

})(window.App);
