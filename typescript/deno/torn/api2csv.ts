/**
 * @file api2csv.ts
 * @description Fetches faction data from the Torn V2 API and writes a summary to a CSV file.
 * @author Gemini
 * @version 1.0.0
 *
 * @example
 * // To run this script, use the Deno runtime:
 * // You must provide your Torn API key and the ID of the faction you want to look up.
 * deno run --allow-net --allow-write api2csv.ts YOUR_API_KEY FACTION_ID
 *
 * @example
 * // e.g., For the faction with ID 13784 and a placeholder API key:
 * deno run --allow-net --allow-write api2csv.ts AbcDeFg123456789 13784
 *
 * @requires Deno
 * @requires --allow-net flag to fetch from the API.
 * @requires --allow-write flag to save the CSV file.
 */

// Import the 'stringify' function from the Deno standard library's CSV module.
// This will help us easily convert our data into a valid CSV format.
import { stringify } from "https://deno.land/std/csv/stringify.ts";

// Define an interface for the expected structure of the API error response.
// This helps with type safety.
interface TornApiError {
  error: {
    code: number;
    error: string;
  };
}

// Define an interface for the expected structure of a successful API response.
// We only define the fields we are interested in for this script.
interface FactionBasicData {
  id: number;
  name: string;
  respect: number;
  leader_id: number;
  members: number;
  best_chain: number;
}
interface FactionBasicResponse {
  basic: FactionBasicData;
}

/**
 * Fetches faction data from the Torn API.
 * @param apiKey - Your personal Torn API key.
 * @returns A Promise that resolves to the faction data.
 * @throws An error if the fetch fails, is not ok, or if the API returns an error.
 */
async function fetchFactionData(apiKey: string): Promise<FactionBasicData> {
  const apiUrl = `https://api.torn.com/v2/faction/basic?key=${apiKey}`;
  console.log(`Fetching data from: ${apiUrl.replace(apiKey, "REDACTED")}`);
  const response = await fetch(apiUrl);

  // Check if the HTTP request itself was successful.
  if (!response.ok) {
    throw new Error(`HTTP error! status: ${response.status} ${response.statusText}`);
  }

  const responseData: FactionBasicResponse | TornApiError = await response.json();
  if ("basic" in responseData) {
    return responseData.basic;
  }

  throw new Error(`Torn API Error: ${responseData.error.error} (Code: ${responseData.error.code})`);
}

/**
 * The main function that orchestrates the script's execution.
 */
async function main() {
  // Validate that the user has provided the necessary arguments.
  if (Deno.args.length != 1) {
    console.error("ERROR: Missing required argument");
    console.error("Usage: deno run --allow-net --allow-write api2csv.ts <YOUR_API_KEY>");
    Deno.exit(1); // Exit with a non-zero code to indicate an error.
  }
  
  const apiKey = Deno.args[0];

  try {
    // Step 1: Fetch the data from the API.
    const factionData = await fetchFactionData(apiKey);
    console.log(`Successfully fetched data for faction: "${factionData.name}"`);

    // Step 2: Process the JSON response to create a summary object.
    const summary = {
      factionId: factionData.id,
      factionName: factionData.name,
      respect: factionData.respect,
      leaderUserId: factionData.leader_id,
      memberCount: factionData.members,
      bestChain: factionData.best_chain,
    };

    // Step 3: Prepare the data for CSV conversion.
    // The stringify function needs an array of arrays or an array of objects.
    // We'll provide the header row and a single data row.
    const csvHeader = [
        "FactionID",
        "FactionName",
        "Respect",
        "LeaderUserID",
        "MemberCount",
        "BestChain"
    ];
    const csvDataRow = [
        summary.factionId,
        summary.factionName,
        summary.respect,
        summary.leaderUserId,
        summary.memberCount,
        summary.bestChain,
    ];

    // Step 4: Convert the data to a CSV formatted string.
    const csvContent = await stringify([csvHeader, csvDataRow]);

    // Step 5: Write the CSV string to a local file.
    const outputFilename = `faction_summary_${summary.factionId}.csv`;
    await Deno.writeTextFile(outputFilename, csvContent);
    console.log(`✅ Success! Summary saved to ${outputFilename}`);
  } catch (error) {
    console.error(`❌ An error occurred: ${error.message}`);
    Deno.exit(1);
  }
}

// This standard Deno entry point check ensures that the main() function
// is called only when the script is executed directly.
if (import.meta.main) {
  main();
}
