<?php

date_default_timezone_set('America/Sao_Paulo');

class Bootstrap extends Zend_Application_Bootstrap_Bootstrap
{
    protected function _initRequest()
    {
        $this->bootstrap('FrontController');
        $front = $this->getResource('FrontController');

        $request = new Zend_Controller_Request_Http();
        $request->setBaseUrl('/');

        $front->setRequest($request);

        return $request;
    }

    protected function _initView()
    {
        Zend_Layout::startMvc();
        $view = new Zend_View();
        $view->doctype('XHTML1_TRANSITIONAL');
        $view->headTitle('zf :: example');
        $view->headLink()->appendStylesheet('/static/style/base.css');
        
        $viewRenderer = Zend_Controller_Action_HelperBroker::getStaticHelper(
            'ViewRenderer'
        );
        $viewRenderer->setView($view);

        return $view;
    }
}
