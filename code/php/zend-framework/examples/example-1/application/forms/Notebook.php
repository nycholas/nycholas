<?php

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

