const { MongoClient } = require('mongodb');
const dotenv = require('dotenv');
const process = require('node:process');
const USER_MONGO_COLLECTION = require('./userCollectionToExist.json');

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

/**
 * Seeds the MongoDB database.
 * @returns {Promise<void>} A promise that resolves when the seeding is complete.
 */
async function seedMongoDB() {
  await seedUsers();
}

async function seedUsers() {
  const client = new MongoClient(testDbUri, { useNewUrlParser: true, useUnifiedTopology: true });

  try {
    await client.connect();

    const dbName = testDbUri.split('/').pop();
    const db = client.db(dbName);

    const users = db.collection('users');

    const usersToCreate = USER_MONGO_COLLECTION;

    usersToCreate.forEach((user) => {
      user.createdAt = new Date();
      user.updatedAt = new Date();
    });
    await users.insertMany(usersToCreate);

    const userCount = await users.countDocuments();
    console.log(`🌱 Test DB (${dbName}) seeded with ${userCount} users!`);
  } catch (err) {
    console.error('An error occurred while seeding the test DB:', err);
  } finally {
    await client.close();
  }
}

seedMongoDB();
