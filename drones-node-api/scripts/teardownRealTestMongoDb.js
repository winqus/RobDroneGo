const { MongoClient } = require('mongodb');
const dotenv = require('dotenv');
const process = require('node:process');

if (!process.env.TEST_MONGODB_URI) {
  const testEnvFound = dotenv.config({ path: '.env.test' });
  if (!testEnvFound) {
    throw new Error("⚠️  Couldn't find .env.test file  ⚠️");
  }
}
const testDbUri = process.env.TEST_MONGODB_URI || undefined;
if (!testDbUri) {
  throw new Error("⚠️  Couldn't find TEST_MONGODB_URI in .env.test file  ⚠️");
}

async function teardown() {
  const client = new MongoClient(testDbUri, { useNewUrlParser: true, useUnifiedTopology: true });

  try {
    await client.connect();

    const dbName = testDbUri.split('/').pop();
    const db = client.db(dbName);

    await db.dropDatabase();
    console.log(`Test DB (${dbName}) discarded!`);
  } catch (err) {
    console.error('An error occurred while tearing down the test DB:', err);
  } finally {
    await client.close();
  }
}
teardown();
