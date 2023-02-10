import { Contract } from './type-aliases';
import { TezosToolkit } from '@taquito/taquito';
import { originateContract } from './ligo';
import {
  BondingCurveCode,
} from '../bin-ts';
import { $log } from '@tsed/logger';


export async function originateBondingCurve(
  tz: TezosToolkit,
  storage: string | Record<string, any>,
): Promise<Contract> {
  $log.info(`originating bonding curve contract..`);
  return originateContract(tz, BondingCurveCode.code, storage, 'bonding-curve');
}


// Convert an int to its superscript form, e.g. 123 -> '¹²³'
function toSuperscript(num: number) {
  const superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹";
  const num_str = num.toString();
  let output = '';
  for (let i = 0; i < num_str.length; i++) {
    output += superscripts[parseInt(num_str[i])];
  }
  return output;
}

// const toSuperscriptTests = [0,1,2,3,4,5,6,7,8,9,10,11,22,154,1234654];
// for (let i = 0; i < toSuperscriptTests.length; i++) {
//   console.log(toSuperscriptTests[i], toSuperscript(toSuperscriptTests[i]));
// }

// // 0 "⁰"
// // ?editor_console=true:57 1 "¹"
// // ?editor_console=true:57 2 "²"
// // ?editor_console=true:57 3 "³"
// // ?editor_console=true:57 4 "⁴"
// // ?editor_console=true:57 5 "⁵"
// // ?editor_console=true:57 6 "⁶"
// // ?editor_console=true:57 7 "⁷"
// // ?editor_console=true:57 8 "⁸"
// // ?editor_console=true:57 9 "⁹"
// // ?editor_console=true:57 10 "¹⁰"
// // ?editor_console=true:57 11 "¹¹"
// // ?editor_console=true:57 22 "²²"
// // ?editor_console=true:57 154 "¹⁵⁴"
// // ?editor_console=true:57 1234654 "¹²³⁴⁶⁵⁴"

// Convert a coefficient list to a Unicode polynomial string
export function toPolynomialUnicode(coefficients: Array<number>) {
  let output = '';
  for (let i = 0; i < coefficients.length; i++) {
    if (i == 0) {
      if (coefficients[i] !== 0) {
        output += `${coefficients[i]} + `;
      }
    } else if (coefficients[i] === 1) {
      output += `x${toSuperscript(i)} + `;
    } else if (coefficients[i] === -1) {
      output = output.replace(/ \+ $/, '');
      output += ` - x${toSuperscript(i)} + `;
    } else if (coefficients[i] !== 0) {
      output += `${coefficients[i]} × x${toSuperscript(i)} + `;
    }
  }
  return output.replace(/ \+ $/, '');
}

// Convert a coefficient list to an ASCII polynomial string
export function toPolynomialAscii(coefficients: Array<number>) {
  let output = '';
  for (let i = 0; i < coefficients.length; i++) {
    if (i == 0) {
      if (coefficients[i] !== 0) {
        output += `${coefficients[i]} + `;
      }
    } else if (coefficients[i] === 1) {
      output += `x^${i} + `;
    } else if (coefficients[i] === -1) {
      output = output.replace(/ \+ $/, '');
      output += ` - x^${i} + `;
    } else if (coefficients[i] !== 0) {
      output += `${coefficients[i]} * x^${i} + `;
    }
  }
  return output.replace(/ \+ $/, '');
}

// let input = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 11, 12];
// let input2 = [-1, -1, -2, -3, -4, -5, -6, 1, -8, -9, -1, -11, -12];

// x¹ + 2 * x² + 3 * x³ + 4 * x⁴ + 5 * x⁵ + 6 * x⁶ + 7 * x⁷ + 8 * x⁸ + 9 * x⁹ + x¹⁰ + 11 * x¹¹ + 12 * x¹²
// console.log(toPolynomialUnicode(input));

// x^1 + 2 * x^2 + 3 * x^3 + 4 * x^4 + 5 * x^5 + 6 * x^6 + 7 * x^7 + 8 * x^8 + 9 * x^9 + x^10 + 11 * x^11 + 12 * x^12
// console.log(toPolynomialAscii(input));

// -1 - x¹ + -2 * x² + -3 * x³ + -4 * x⁴ + -5 * x⁵ + -6 * x⁶ + x⁷ + -8 * x⁸ + -9 * x⁹ - x¹⁰ + -11 * x¹¹ + -12 * x¹²
// console.log(toPolynomialUnicode(input2));

// -1 - x^1 + -2 * x^2 + -3 * x^3 + -4 * x^4 + -5 * x^5 + -6 * x^6 + x^7 +
// -8 * x^8 + -9 * x^9 - x^10 + -11 * x^11 + -12 * x^12
// console.log(toPolynomialAscii(input2));


export {
  BondingCurveCode,
  BondingCurveContractType,
} from '../bin-ts';

