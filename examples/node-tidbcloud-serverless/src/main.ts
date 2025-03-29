import { connect } from "@tidbcloud/serverless";

import {
  createAuthor,
  deleteAuthor,
  getAuthor,
  listAuthors,
} from "./db/query_sql";

interface Author {
  id: string;
  name: string;
  bio: string | null;
}

async function main() {
  const url = process.env["DATABASE_URL"] ?? "";

  // Connect to TiDB Cloud Serverless
  const client = connect({ url });

  // Create an author
  await createAuthor(client, {
    name: "Seal",
    bio: "Kissed from a rose",
  });

  // List the authors
  const authors = await listAuthors(client);
  console.log(authors);

  // Get that author
  const seal = await getAuthor(client, { id: authors[0].id });
  if (seal === null) {
    throw new Error("seal not found");
  }
  console.log(seal);

  // Delete the author
  await deleteAuthor(client, { id: seal.id });
}

(async () => {
  try {
    await main();
    process.exit(0);
  } catch (e) {
    console.error(e);
    process.exit(1);
  }
})();
