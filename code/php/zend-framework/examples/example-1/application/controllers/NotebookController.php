<?php
/**
 * Simple example Zendframework.
 * Copyright (c) 2011, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *  * Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
 *    its contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

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
