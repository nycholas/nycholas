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

class Application_Form_Notebook extends Zend_Form
{

    public function init()
    {
        $this->setName('notebook');
        $this->setMethod('post');

        $this->addElement('hidden', 'id', array(
            'filters' => array('Int')
        ));

        $this->addElement('text', 'title', array(
            'label' => 'Title:',
            'required' => true,
            'filters' => array('StringTrim'),
            'validators' => array(
                array(
                    'validator' => 'StringLength',
                    'options' => array(0, 45)
                )
            )
        ));

        $this->addElement('textarea', 'description', array(
            'label' => 'Description:',
            'required' => false,
            'filters' => array('StringTrim')
        ));

        $this->addElement('text', 'dateJoined', array(
            'label' => 'Date Joined:',
            'required' => true,
            'value' => date('Y-m-d H:i:s')
        ));

        $this->addElement('checkbox', 'status', array(
            'label' => 'Status:',
            'required' => true,
            'value' => 1,
        ));

        $this->addElement('submit', 'submit', array(
            'label' => 'Save',
            'ignore' => true,
        ));

        $this->addElement('hash', 'csrf', array(
            'ignore' => true,
        ));
    }
}

