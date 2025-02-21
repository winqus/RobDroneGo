import { Name } from '../ValueObjects/name';

describe('Name', () => {
  it('should successfully create a Name instance with valid strings', () => {
    const validNameString = 'ValidName';
    const nameResult = Name.create(validNameString);
    expect(nameResult.isSuccess).toBe(true);
    expect(nameResult.getValue().value).toBe(validNameString);
  });

  it('should fail for empty, null or undefined names', () => {
    [null, undefined, ''].forEach((invalidName) => {
      const nameResult = Name.create(invalidName as string);
      expect(nameResult.isFailure).toBe(true);
    });
  });

  it('should fail for non-alphanumeric names', () => {
    const invalidNameString = 'Invalid Name!';
    const nameResult = Name.create(invalidNameString);
    expect(nameResult.isFailure).toBe(true);
  });

  it('should fail for names longer than 50 characters', () => {
    const longNameString = 'a'.repeat(51);
    const nameResult = Name.create(longNameString);
    expect(nameResult.isFailure).toBe(true);
  });
});
