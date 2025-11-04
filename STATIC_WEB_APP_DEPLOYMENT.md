# Azure Static Web App Deployment Guide

This guide covers deploying the AskOTIS frontend to Azure Static Web Apps with Azure AD authentication.

## Prerequisites

- Azure subscription with appropriate permissions
- Azure CLI installed and authenticated
- Node.js and npm installed
- Static Web Apps CLI (`npm install -g @azure/static-web-apps-cli`)

## 1. Create Azure Static Web App Resource

### Option A: Using Azure Portal

1. Navigate to [Azure Portal](https://portal.azure.com)
2. Click **"Create a resource"** → Search for **"Static Web App"**
3. Fill in the details:
   - **Subscription**: Select your subscription
   - **Resource Group**: Choose existing or create new (e.g., `Azure_AI_RG`)
   - **Name**: `AskOTIS` (or your preferred name)
   - **Plan type**: Free (for development) or Standard (for production)
   - **Region**: East US 2 (or closest to your users)
   - **Deployment source**: Other (we'll deploy manually)
4. Click **"Review + Create"** → **"Create"**

### Option B: Using Azure CLI

```bash
az staticwebapp create \
  --name AskOTIS \
  --resource-group Azure_AI_RG \
  --location "East US 2" \
  --sku Free
```

## 2. Get Deployment Token

The deployment token is required for manual deployments.

### Using Azure Portal:
1. Navigate to your Static Web App resource
2. Go to **Settings** → **Deployment tokens**
3. Copy the deployment token

### Using Azure CLI:
```bash
az staticwebapp secrets list --name AskOTIS --query "properties.apiKey" --output tsv
```

Save this token securely - you'll need it for deployments.

## 3. Configure Azure AD Authentication

### Create Azure AD App Registration

1. Navigate to **Azure Active Directory** → **App registrations**
2. Click **"+ New registration"**
3. Fill in the details:
   - **Name**: `AskOTIS-Auth` (or your preferred name)
   - **Supported account types**: Single tenant (your organization only)
   - **Redirect URI**: Web → `https://YOUR-STATIC-WEB-APP-URL/.auth/login/aad/callback`
4. Click **"Register"**

### Configure App Registration

1. **Authentication Settings**:
   - Go to **Authentication** in the left menu
   - Under **Implicit grant and hybrid flows**, check:
     - ☑️ **ID tokens (used for implicit and hybrid flows)**
   - Click **"Save"**

2. **Create Client Secret**:
   - Go to **Certificates & secrets** in the left menu
   - Click **"+ New client secret"**
   - Add a description (e.g., "AskOTIS Production")
   - Set expiration (e.g., 24 months)
   - Click **"Add"**
   - **Copy the secret value immediately** (you won't be able to see it again)

3. **Note the following values**:
   - **Application (client) ID** from the Overview page
   - **Directory (tenant) ID** from the Overview page
   - **Client secret value** you just created

### Configure Static Web App Environment Variables

Add the Azure AD credentials as environment variables in your Static Web App:

```bash
az staticwebapp appsettings set \
  --name AskOTIS \
  --setting-names \
    AZURE_CLIENT_ID="<your-client-id>" \
    AZURE_CLIENT_SECRET="<your-client-secret>"
```

Or via Azure Portal:
1. Navigate to your Static Web App
2. Go to **Configuration** → **Environment variables**
3. Add the following variables:
   - `AZURE_CLIENT_ID`: Your app registration client ID
   - `AZURE_CLIENT_SECRET`: Your app registration client secret
4. Click **"Save"**

### Update Redirect URIs

Once your Static Web App is created, update the redirect URI in your App Registration:

1. Go to your App Registration → **Authentication**
2. Update the redirect URI to match your actual Static Web App URL:
   - `https://YOUR-ACTUAL-STATIC-WEB-APP-URL/.auth/login/aad/callback`
3. Click **"Save"**

### Grant Admin Consent

If your organization requires admin consent:

1. Go to **API permissions** in your App Registration
2. Click **"Grant admin consent for [Your Organization]"**
3. Confirm the consent

Or use this direct consent URL:
```
https://login.microsoftonline.com/{tenant-id}/adminconsent?client_id={client-id}
```

## 4. Configure Authentication in staticwebapp.config.json

Create or update `deploy/staticwebapp.config.json`:

```json
{
  "routes": [
    {
      "route": "/*",
      "allowedRoles": ["authenticated"]
    }
  ],
  "responseOverrides": {
    "401": {
      "redirect": "/.auth/login/aad",
      "statusCode": 302
    }
  },
  "auth": {
    "identityProviders": {
      "azureActiveDirectory": {
        "registration": {
          "openIdIssuer": "https://login.microsoftonline.com/{tenant-id}/v2.0",
          "clientIdSettingName": "AZURE_CLIENT_ID",
          "clientSecretSettingName": "AZURE_CLIENT_SECRET"
        },
        "login": {
          "loginParameters": [
            "domain_hint=yourorganization.com"
          ]
        }
      }
    }
  },
  "globalHeaders": {
    "X-Frame-Options": "DENY",
    "X-Content-Type-Options": "nosniff"
  },
  "navigationFallback": {
    "rewrite": "/index.html",
    "exclude": ["/*.{css,js,jpg,png,gif,svg,ico,woff,woff2,ttf,eot}"]
  }
}
```

Replace:
- `{tenant-id}`: Your Azure AD tenant ID
- `yourorganization.com`: Your organization's domain

## 5. Deploy to Azure Static Web Apps

### Manual Deployment using SWA CLI

#### PowerShell (Recommended - Auto-retrieves token from Azure)

```powershell
# Deploy using Azure CLI to automatically retrieve deployment token
$token = az staticwebapp secrets list --name AskOTIS --query "properties.apiKey" --output tsv
npx @azure/static-web-apps-cli deploy ./deploy --deployment-token $token
```

#### Bash (Manual token)

```bash
# Install SWA CLI globally (if not already installed)
npm install -g @azure/static-web-apps-cli

# Get deployment token from Azure CLI
TOKEN=$(az staticwebapp secrets list --name AskOTIS --query "properties.apiKey" --output tsv)

# Deploy to production
npx @azure/static-web-apps-cli deploy ./deploy --deployment-token "$TOKEN"
```

#### Using Environment Variable (Alternative)

```powershell
# Set token as environment variable
$env:AZURE_STATIC_WEB_APPS_API_TOKEN = $(az staticwebapp secrets list --name AskOTIS --query "properties.apiKey" --output tsv)

# Deploy using environment variable
npx @azure/static-web-apps-cli deploy ./deploy --deployment-token "$env:AZURE_STATIC_WEB_APPS_API_TOKEN"
```

### Deployment from Git Repository

1. Navigate to your Static Web App in Azure Portal
2. Go to **Settings** → **Deployment**
3. Connect to your GitHub or Azure DevOps repository
4. Configure build settings:
   - **App location**: `/deploy`
   - **Api location**: (leave empty if no API)
   - **Output location**: (leave empty)

The app will automatically deploy on every push to the configured branch.

## 6. Configure Static Web App Settings

### Disable Preview Environments (Optional)

To prevent unauthorized access through preview environments:

1. Navigate to your Static Web App in Azure Portal
2. Go to **Configuration**
3. Under **Preview environments**:
   - ☐ Uncheck **"Enable preview environments"**
4. Click **"Save"**

### Enable Configuration File Changes

Ensure configuration file changes are enabled:

1. Navigate to your Static Web App in Azure Portal
2. Go to **Configuration**
3. Under **Configuration file**:
   - ☑️ Check **"Enable configuration file changes"**
4. Click **"Save"**

## 7. Verify Deployment

### Test Authentication Flow

1. Open your Static Web App URL in an incognito/private browser window
2. You should be redirected to the Azure AD login page
3. Sign in with your organizational credentials
4. After successful authentication, you should be redirected to the AskOTIS application

### Check Authentication Status

You can verify authentication status by visiting:
```
https://YOUR-STATIC-WEB-APP-URL/.auth/me
```

This should return your user profile information if authenticated.

### Test Unauthenticated Access

```bash
# Should return 302 redirect to login
curl -I https://YOUR-STATIC-WEB-APP-URL/
```

## 8. Troubleshooting

### "We couldn't sign you in" Error

**Common causes:**
1. **Missing ID tokens**: Ensure "ID tokens" is checked in App Registration → Authentication
2. **Missing admin consent**: Grant admin consent in App Registration → API permissions
3. **Incorrect redirect URI**: Verify the redirect URI matches exactly in App Registration
4. **Missing environment variables**: Check AZURE_CLIENT_ID and AZURE_CLIENT_SECRET are set

### Authentication Not Enforced

**Solutions:**
1. Verify `staticwebapp.config.json` is in the deploy folder
2. Ensure "Enable configuration file changes" is checked in Azure Portal
3. Wait a few minutes for configuration to propagate
4. Clear browser cache and try in incognito mode

### Preview Environment Not Secured

**Solution:**
1. Disable preview environments in Azure Portal → Configuration
2. Delete any existing preview environments

## 9. Updating the Deployment

To update your application:

```bash
# Deploy updated files
npx @azure/static-web-apps-cli deploy ./deploy \
  --deployment-token <your-deployment-token> \
  --env production
```

Configuration changes take effect immediately without requiring redeployment.

## 10. Security Best Practices

1. **Never commit secrets to git**:
   - Use `.env.template` files with placeholder values
   - Add `.env`, `local.settings.json`, and `*.zip` to `.gitignore`

2. **Rotate keys regularly**:
   - Rotate client secrets annually or when compromised
   - Rotate deployment tokens if exposed

3. **Use managed identities** where possible:
   - For Azure Functions connecting to other Azure services
   - Eliminates need for connection strings in many cases

4. **Monitor for exposed secrets**:
   - Enable GitGuardian or similar secret scanning
   - Immediately rotate any exposed keys

5. **Restrict access**:
   - Use single-tenant authentication for organizational apps
   - Limit API permissions to only what's needed
   - Enable preview environment restrictions

## Resources

- [Azure Static Web Apps Documentation](https://docs.microsoft.com/azure/static-web-apps/)
- [Azure AD Authentication Configuration](https://docs.microsoft.com/azure/static-web-apps/authentication-authorization)
- [Static Web Apps CLI](https://azure.github.io/static-web-apps-cli/)
- [Configuration Reference](https://docs.microsoft.com/azure/static-web-apps/configuration)

## Support

For issues or questions:
- Check Azure Portal → Static Web Apps → Monitoring → Logs
- Review authentication errors in browser developer console
- Contact your Azure administrator for permission-related issues
