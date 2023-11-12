import { Pipe, PipeTransform } from '@angular/core';

@Pipe({ name: 'errorMessage' })
export class ErrorMessagePipe implements PipeTransform {
  transform(value: string, args: { [key: string]: any }): string {
    if (!value) return value;

    Object.keys(args).forEach(key => {
      value = value.replace(new RegExp(`{${key}}`, 'g'), args[key]);
    });

    return value;
  }
}