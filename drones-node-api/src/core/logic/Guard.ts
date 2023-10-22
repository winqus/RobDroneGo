export interface IGuardResult {
  succeeded: boolean;
  message?: string;
}

export interface IGuardArgument {
  argument: any;
  argumentName: string;
}

export type GuardArgumentCollection = IGuardArgument[];

export class Guard {
  public static combine(guardResults: IGuardResult[]): IGuardResult {
    for (const result of guardResults) {
      if (result.succeeded === false) {
        return result;
      }
    }

    return { succeeded: true };
  }

  public static againstNullOrUndefined(argument: any, argumentName: string): IGuardResult {
    if (argument === null || argument === undefined) {
      return { succeeded: false, message: `${argumentName} is null or undefined` };
    } else {
      return { succeeded: true };
    }
  }

  public static againstNullOrUndefinedBulk(args: GuardArgumentCollection): IGuardResult {
    for (const arg of args) {
      const result = this.againstNullOrUndefined(arg.argument, arg.argumentName);
      if (!result.succeeded) {
        return result;
      }
    }

    return { succeeded: true };
  }

  public static isOneOf(value: any, validValues: any[], argumentName: string): IGuardResult {
    let isValid = false;
    for (const validValue of validValues) {
      if (value === validValue) {
        isValid = true;
      }
    }

    if (isValid) {
      return { succeeded: true };
    } else {
      return {
        succeeded: false,
        message: `${argumentName} isn't oneOf the correct types in ${JSON.stringify(validValues)}. Got "${value}".`,
      };
    }
  }

  public static inRange(num: number, min: number, max: number, argumentName: string): IGuardResult {
    const isInRange = num >= min && num <= max;
    if (!isInRange) {
      return { succeeded: false, message: `${argumentName} is not within range ${min} to ${max}.` };
    } else {
      return { succeeded: true };
    }
  }

  public static allInRange(numbers: number[], min: number, max: number, argumentName: string): IGuardResult {
    let failingResult: IGuardResult = null;
    for (const num of numbers) {
      const numIsInRangeResult = this.inRange(num, min, max, argumentName);
      if (!numIsInRangeResult.succeeded) {
        failingResult = numIsInRangeResult;
      }
    }

    if (failingResult) {
      return { succeeded: false, message: `${argumentName} is not within the range.` };
    } else {
      return { succeeded: true };
    }
  }

  public static isAlphanumeric(value: string, argumentName: string): IGuardResult {
    const isAlphanumericRegex = /^[a-z0-9]+$/i;
    const isValid = isAlphanumericRegex.test(value);
    if (!isValid) {
      return { succeeded: false, message: `${argumentName} can only contain alphanumeric characters` };
    } else {
      return { succeeded: true };
    }
  }

  public static isAlphanumericWithSpaces(value: string, argumentName: string): IGuardResult {
    const isAlphanumericRegex = /^[a-z0-9\s]+$/i;
    const isValid = isAlphanumericRegex.test(value);
    if (!isValid) {
      return { succeeded: false, message: `${argumentName} can only contain alphanumeric characters and spaces` };
    } else {
      return { succeeded: true };
    }
  }

  public static isOfLength(value: string, minLength: number, maxLength: number, argumentName: string): IGuardResult {
    const length = value?.length;
    if (length < minLength || length > maxLength) {
      return {
        succeeded: false,
        message: `${argumentName} length should be between ${minLength} and ${maxLength} characters`,
      };
    } else {
      return { succeeded: true };
    }
  }

  public static isTrue(predicate: boolean, message: string): IGuardResult {
    if (!predicate) {
      return { succeeded: false, message: message };
    } else {
      return { succeeded: true };
    }
  }
}
