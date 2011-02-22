<?php

class Application_Model_Notebook
{
    protected $_id;
    protected $_title;
    protected $_description;
    protected $_dateJoined;
    protected $_status;

    public function __construct(array $options = null)
    {
        if (is_array($options)) {
            $this->setOptions($options);
        }
    }

    public function __set($name, $value)
    {
        $method = 'set' . $name;
        if (('mapper' == $name) || !method_exists($this, $method)) {
            throw new Exception('Invalid notebook property');
        }
        $this->$method($value);
    }

    public function __get($name)
    {
        $method = 'get' . $name;
        if (('mapper' == $name) || !method_exists($this, $method)) {
            throw new Exception('Invalid notebook property');
        }
        return $this->$method();
    }

    public function toArray()
    {
        return array(
            'id' => $this->_id,
            'title' => $this->_title,
            'description' => $this->_description,
            'date_joined' => $this->_dateJoined,
            'status' => $this->_status,
        );
    }

    public function setOptions(array $options)
    {
        $methods = get_class_methods($this);
        foreach ($options as $key => $value) {
            $method = 'set' . ucfirst($key);
            if (in_array($method, $methods)) {
                $this->$method($value);
            }
        }
        return $this;
    }

    public function setId($id)
    {
        $this->_id = (int) $id;
        return $this;
    }

    public function getId()
    {
        return $this->_id;
    }

    public function setTitle($title)
    {
        $this->_title = $title;
        return $this;
    }

    public function getTitle()
    {
        return $this->_title;
    }

    public function setDescription($description)
    {
        $this->_description = $description;
        return $this;
    }

    public function getDescription()
    {
        return $this->_description;
    }

    public function setDateJoined($dateJoined)
    {
        $this->_dateJoined = $dateJoined;
        return $this;
    }

    public function getDateJoined()
    {
        return $this->_dateJoined;
    }

    public function setStatus($status)
    {
        $this->_status = (int) $status;
        return $this;
    }

    public function getStatus()
    {
        return $this->_status;
    }
}
