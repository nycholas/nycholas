<?php

class NotebookController extends Zend_Controller_Action
{

    public function init()
    {
        /* Initialize action controller here */
    }

    public function indexAction()
    {
        $response = $this->getResponse();

        $mapper = new Application_Model_NotebookMapper();
        $this->view->entries = $mapper->fetchAll();
    }

    public function addAction()
    {
        $request = $this->getRequest();

        $form = new Application_Form_Notebook();
        $form->submit->setLabel('Add');

        if ($this->getRequest()->isPost()) {
            if ($form->isValid($request->getPost())) {
                $model = new Application_Model_Notebook($form->getValues());
                $mapper = new Application_Model_NotebookMapper();
                $mapper->save($model);
                return $this->_helper->redirector('index');
            } else {
                $form->populate($formData);
            }
        }

        $this->view->form = $form;
    }

    public function editAction()
    {
        $request = $this->getRequest();
        $id = (int) $request->getParam('id', 0);

        $form = new Application_Form_Notebook();
        $form->submit->setLabel('Save');

        $mapper = new Application_Model_NotebookMapper();

        if ($this->getRequest()->isPost()) {
            if ($form->isValid($request->getPost())) {
                $model = new Application_Model_Notebook($form->getValues());
                $mapper->save($model);
                return $this->_helper->redirector('index');
            }
        } else {
            if ($id > 0) {
                $model = $mapper->find($id);
                $form->populate($model->toArray());
            }
        }

        $this->view->form = $form;
    }

    public function deleteAction()
    {
        $request = $this->getRequest();
        $id = (int) $request->getParam('id', 0);

        $mapper = new Application_Model_NotebookMapper();
        $mapper->delete($id);

        return $this->_helper->redirector('index');
    }

    public function statusAction()
    {
        $request = $this->getRequest();
        $id = (int) $request->getParam('id', 0);

        $mapper = new Application_Model_NotebookMapper();
        $model = $mapper->find($id);
        $model->status = !$model->status;
        $mapper->save($model);

        return $this->_helper->redirector('index');
    }
}
