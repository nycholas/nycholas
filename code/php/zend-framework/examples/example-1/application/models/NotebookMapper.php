<?php

class Application_Model_NotebookMapper
{
    protected $_dbTable;

    public function setDbTable($dbTable)
    {
        if (is_string($dbTable)) {
            $dbTable = new $dbTable();
        }

        if (!$dbTable instanceof Zend_Db_Table_Abstract) {
            throw new Exception('Invalid table data gateway provided');
        }

        $this->_dbTable = $dbTable;

        return $this;
    }

    public function getDbTable()
    {
        if (null === $this->_dbTable) {
            $this->setDbTable('Application_Model_DbTable_Notebook');
        }

        return $this->_dbTable;
    }

    public function save(Application_Model_Notebook $notebook)
    {
        $data = array(
            'title' => $notebook->getTitle(),
            'description' => $notebook->getDescription(),
            'date_joined' => $notebook->getDateJoined(),
            'status' => $notebook->getStatus()
        );

        if (null === ($id = $notebook->getId())) {
            unset($data['id']);
            $this->getDbTable()->insert($data);
        } else {
            $this->getDbTable()->update($data, array('id = ?' => $id));
        }
    }

    public function delete($id)
    {
        $this->getDbTable()->delete(array('id = ?' => $id));
    }

    public function find($id)
    {
        $result = $this->getDbTable()->find($id);
        if (0 == count($result)) {
            return;
        }

        $row = $result->current();

        $notebook = new Application_Model_Notebook();
        $notebook->setId($row->id)
                 ->setTitle($row->title)
                 ->setDescription($row->description)
                 ->setDateJoined($row->date_joined)
                 ->setStatus($row->status);
        return $notebook;
    }

    public function fetchAll()
    {
        $resultSet = $this->getDbTable()->fetchAll();
        $entries = array();
        foreach ($resultSet as $row) {
            $entry = new Application_Model_Notebook();
            $entry->setId($row->id)
                  ->setTitle($row->title)
                  ->setDescription($row->description)
                  ->setDateJoined($row->date_joined)
                  ->setStatus($row->status);
            $entries[] = $entry;
        }

        return $entries;
    }
}

