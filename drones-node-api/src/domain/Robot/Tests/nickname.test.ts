import { Nickname } from '../ValueObjects/nickname';

describe('Nickname', () => {
  describe('create', () => {
    it.each(['A', 'A1b', 'a1B2C'])(
      'should successfully create a Nickname instance with valid alphanumeric strings of length between 1 to 30',
      (validNickname) => {
        // Act
        const nicknameResult = Nickname.create(validNickname);

        // Assert
        expect(nicknameResult.isSuccess).toBe(true);
        expect(nicknameResult.getValue()).toBeInstanceOf(Nickname);
        expect(nicknameResult.getValue().value).toBe(validNickname.trim());
      },
    );
    it('should fail if the nickname length is more than 30 characters', () => {
      // Arrange
      const longNickname = 'a'.repeat(31);

      // Act
      const nicknameResult = Nickname.create(longNickname);

      // Assert
      expect(nicknameResult.isFailure).toBe(true);
      expect(nicknameResult.error).toMatch(/between/);
    });

    it('should trim the nickname and succeed if it is otherwise valid', () => {
      // Arrange
      const nicknameWithSpaces = '  Nickname   ';
      const expectedNickname = 'Nickname';

      // Act
      const nicknameResult = Nickname.create(nicknameWithSpaces);

      // Assert
      expect(nicknameResult.isSuccess).toBe(true);
      expect(nicknameResult.getValue().value).toBe(expectedNickname);
    });

    it.each(['A@1', 'b-2', 'AB!'])('should fail if the nickname contains special characters', (specialNickname) => {
      // Act
      const nicknameResult = Nickname.create(specialNickname);

      // Assert
      expect(nicknameResult.isFailure).toBe(true);
      expect(nicknameResult.error).toMatch(/alphanumeric/);
    });
  });
});
