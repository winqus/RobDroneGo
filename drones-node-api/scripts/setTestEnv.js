const fs = require('fs');
const dotenv = require('dotenv');

const envFound = dotenv.config();
dotenv.config({ path: '.env.development' });

process.env.NODE_ENV = 'test';

if (!envFound || !process.env.TEST_MONGODB_BASE_URI) {
  process.env.TEST_MONGODB_BASE_URI = 'mongodb://127.0.0.1:27017/test';
}

const testDbURL = new URL(process.env.TEST_MONGODB_BASE_URI);
testDbURL.pathname = (testDbURL.pathname || 'test') + '-' + new Date().toISOString().replace(/[^a-zA-Z0-9]/g, '');

const path = '.env.test';

const testDbUri = testDbURL.toString();
const newLine = `TEST_MONGODB_URI=${testDbUri}`;

fs.readFile(path, 'utf8', function(err, data) {
  if (err) {
    if (err.code === 'ENOENT') {
      return fs.writeFile(path, newLine, function(writeErr) {
        if (writeErr) {
          throw writeErr;
        }
        console.log('.env.test file has been created!');
      });
    }
    throw err;
  }

  let lines = data.split('\n').filter(Boolean);

  lines = lines.filter((line) => !line.startsWith('TEST_MONGODB_URI='));

  lines.push(newLine);

  fs.writeFile(path, lines.join('\n'), function(writeErr) {
    if (writeErr) {
      throw writeErr;
    }
    console.log('.env.test file has been updated!');
  });
});
console.log('TEST_MONGODB_URI=', testDbUri);
