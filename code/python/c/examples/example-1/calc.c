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
#include <stdio.h>

int calc_sum(int, int);
int calc_subtract(int, int);
int calc_multiplies(int, int);
int calc_divides(int, int);

int main(void) {
    int a = 6;
    int b = 2;
    printf(" a = %d; b = %d;\n", a, b);
    printf("==============\n\n");
    printf(" + sum: %d\n", calc_sum(a, b));
    printf(" + subtract: %d\n", calc_subtract(a, b));
    printf(" + multiplies: %d\n", calc_multiplies(a, b));
    printf(" + divides: %d\n", calc_divides(a, b));
    return 0;
}

int calc_sum(int a, int b) {
    return a + b;
}

int calc_subtract(int a, int b) {
    return a - b;
}

int calc_multiplies(int a, int b) {
    return a * b;
}

int calc_divides(int a, int b) {
    return a / b;
}