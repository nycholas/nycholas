/**
 * A simple calculator that works with whole numbers written in C/Python.
 * Copyright (C) 2009 by Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 *
 * This file is part of Simple Calculator.
 *
 * Simple Calculator is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdlib.h>
#include <python2.6/Python.h>

static PyObject *pycalc_sum(PyObject *self, PyObject *args) {
    int a, b;
    if (!PyArg_ParseTuple(args, "ii", &a, &b))
        return NULL;
    return Py_BuildValue("i", (a + b));
}

static PyObject *pycalc_subtract(PyObject *self, PyObject *args) {
    int a, b;
    if (!PyArg_ParseTuple(args, "ii", &a, &b))
        return NULL;
    return Py_BuildValue("i", (a - b));
}

static PyObject *pycalc_multiplies(PyObject *self, PyObject *args) {
    int a, b;
    if (!PyArg_ParseTuple(args, "ii", &a, &b))
        return NULL;
    return Py_BuildValue("i", (a * b));
}

static PyObject *pycalc_divides(PyObject *self, PyObject *args) {
    int a, b;
    if (!PyArg_ParseTuple(args, "ii", &a, &b))
        return NULL;
    return Py_BuildValue("i", (a / b));
}

static PyMethodDef pycalc_methods[] = {
    {"sum", pycalc_sum, METH_VARARGS, "Sum two integers."},
    {"subtract", pycalc_subtract, METH_VARARGS, "Subtracts two integers."},
    {"multiplies", pycalc_multiplies, METH_VARARGS, "Subtracts two integers."},
    {"divides", pycalc_divides, METH_VARARGS, "Divide two integers."},
    {NULL, NULL, 0, NULL}
};

void initpycalc(void) {
    PyObject *m;
    m = Py_InitModule("pycalc", pycalc_methods);
    if (m == NULL)
        return;
}

int main(int argc, char *argv[]) {
    Py_SetProgramName(argv[0]);
    Py_Initialize();
    initpycalc();
    return 0;
}