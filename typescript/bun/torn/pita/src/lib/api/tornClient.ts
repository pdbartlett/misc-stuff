export class TornClient {
  private apiKey: string;
  private static readonly API_URL = 'https://api.torn.com';

  constructor(apiKey: string) {
    if (!apiKey) {
      throw new Error('Torn API key is required for TornClient.');
    }
    this.apiKey = apiKey;
  }

  private async fetchData(path: string, params: Record<string, string> = {}) {
    const urlParams = new URLSearchParams({ ...params, key: this.apiKey });
    const url = `${TornClient.API_URL}/${path}?${urlParams.toString()}`;

    try {
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }
      const data = await response.json();
      if (data.error) {
        throw new Error(`Torn API Error (Code ${data.error.code}): ${data.error.error}`);
      }
      return data;
    } catch (error) {
      console.error(`[TornClient] Failed to fetch from ${path}:`, error);
      throw error;
    }
  }

  // V1 "selections" based method (e.g., faction data)
  public async get<T = any>(endpoint: string, id: string, selections: string[]): Promise<T> {
    const path = `${endpoint}/${id}`;
    return this.fetchData(path, { selections: selections.join(',') });
  }

  // V2 "endpoint" based method (covers most new calls)
  public async call<T = any>(endpoint: string, params: Record<string, string> = {}): Promise<T> {
    return this.fetchData(endpoint, params);
  }
  
  // Specific helpers
  public async getFactionBasic() {
    return this.call('faction', { selections: 'basic' });
  }

  public async getFactionMembers() {
    const factionData = await this.getFactionBasic();
    const members = factionData.members;
    // Convert members object to array
    return Object.entries(members).map(([id, memberData]) => ({
      id,
      ...memberData as any,
    }));
  }
}
