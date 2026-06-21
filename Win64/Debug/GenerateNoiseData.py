import tellurium as te
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# 1. Setup model and simulate
r = te.loada("""
    A -> B; k1*A
    B -> C; k2*B
    A = 10; B = 0; C = 0
    k1 = 0.35; k2 = 0.2
""")
data = r.simulate(0, 20, 20)

# 2. Create DataFrames
# df_clean holds the original data
df_clean = pd.DataFrame(data, columns=data.colnames)
# df_noisy is a copy that we will modify
df_noisy = df_clean.copy()

species_cols = [c for c in df_clean.columns if c != 'time']
noise_level = 0.05

# 3. Apply noise to the noisy DataFrame only
for col in species_cols:
    noise = np.random.normal(0, noise_level * df_noisy[col].max(), len(df_noisy))
    df_noisy[col] = (df_noisy[col] + noise).clip(lower=0)

# 4. Plotting comparison
plt.figure(figsize=(12, 8))
for col in species_cols:
    # Plot clean data with a solid line
    plt.plot(df_clean['time'], df_clean[col], linestyle='-', label=f'{col} (Clean)')
    # Plot noisy data with a dashed line or markers
    plt.plot(df_noisy['time'], df_noisy[col], linestyle='--', alpha=0.7, label=f'{col} (Noisy)')

plt.xlabel('Time')
plt.ylabel('Concentration')
plt.title('Comparison: Clean vs. Noisy Biochemical Simulation')
plt.legend()
plt.grid(True)
plt.show()

# 5. Optional: Save noisy files as requested previously
for col in species_cols:
    df_noisy[['time', col]].to_csv(f'noisy_data_{col}.csv', index=False)
    
df_noisy.to_csv('noisy_data.all.csv', index=False)
print("Combined noisy data saved to 'noisy_data.all.csv'")