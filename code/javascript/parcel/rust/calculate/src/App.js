import React, { useState } from 'react';

import Card from '@material-ui/core/Card';
import Alert from '@material-ui/lab/Alert';
import Button from '@material-ui/core/Button';
import Select from '@material-ui/core/Select';
import MenuItem from '@material-ui/core/MenuItem';
import Snackbar from '@material-ui/core/Snackbar';
import Container from '@material-ui/core/Container';
import TextField from '@material-ui/core/TextField';
import { makeStyles } from '@material-ui/core/styles';
import InputLabel from '@material-ui/core/InputLabel';
import Typography from '@material-ui/core/Typography';
import CardContent from '@material-ui/core/CardContent';
import FormControl from '@material-ui/core/FormControl';

import { add, subtract, multiply, divide } from './rust/calculate.rs';

const useStyles = makeStyles(theme => ({
  root: {
    flexGrow: 1,
  },
  title: {
    fontSize: 14,
  },
  formControl: {
    margin: theme.spacing(1),
    minWidth: 120,
  },
  card: {
    marginTop: 180,
    minWidth: 275,
  },
}));

const operations = {
  '+': add,
  '-': subtract,
  '*': multiply,
  '/': divide,
}

function App() {
  const classes = useStyles();

  const [a, setA] = useState(0);
  const [b, setB] = useState(0);
  const [operation, setOperation] = useState('');
  const [result, setResult] = useState(0);
  const [alert, setAlert] = useState({ type: 'success', message: '' });

  const handleCalculate = () => {
    if (!(operation in operations)) {
      setAlert({ type: 'error', message: 'Invalid operation' });
      return;
    }

    try {
      setResult(operations[operation](a, b));
    } catch (e) {
      console.error(e);
      setAlert({ type: 'error', message: 'Wrong operation' })
    }
  };

  const handleAlertClose = (event, reason) => {
    if (reason === 'clickaway') {
      return;
    }
    setAlert({});
  };

  return (
    <Container>
      <Card className={classes.card}>
        <CardContent>
          <Typography className={classes.title} color="textSecondary" gutterBottom>
            Parcel + ReactJS + Rust :: Calculate
          </Typography>
          <FormControl className={classes.formControl}>
            <TextField type="number" id="a" label="a" value={a} onChange={(event) => setA(event.target.value)} />
          </FormControl>
          <FormControl className={classes.formControl}>
            <InputLabel id="operation-select-label">Operation</InputLabel>
            <Select
              labelId="operation-select-label"
              id="operation-select"
              value={operation}
              onChange={(event) => setOperation(event.target.value)}
            >
              {Object.keys(operations).map((op) =>
                <MenuItem key={op} value={op}>{op}</MenuItem>
              )}
            </Select>
          </FormControl>
          <FormControl className={classes.formControl}>
            <TextField type="number" id="b" label="b" value={b} onChange={(event) => setB(event.target.value)} />
          </FormControl>
          <FormControl className={classes.formControl}>
            <Button variant="contained" size="large" color="primary" onClick={handleCalculate}>Calculate</Button>
          </FormControl>
          <FormControl className={classes.formControl}>
            <Typography variant="h1" component="h2">= {result}</Typography>
          </FormControl>
        </CardContent>
      </Card>
      <Snackbar open={!!alert.message} autoHideDuration={6000} onClose={handleAlertClose}>
        <Alert onClose={handleAlertClose} color={alert.type}>
          {alert.message}
        </Alert>
      </Snackbar>
    </Container>
  );
}

export default App;
